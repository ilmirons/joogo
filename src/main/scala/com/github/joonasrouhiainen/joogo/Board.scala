package com.github.joonasrouhiainen.joogo

import scala._
import scala.annotation.tailrec

/**
 * Represents a rectangular go board with a minimum size of 1x1.
 *
 * @author Joonas Rouhiainen
 */
case class Board private(intersections:     Seq[Seq[Option[Color]]],
                         capturesForColors: Map[Color, Int],
                         whoseTurn:         Color,
                         koPosition:        Option[Coords]) {

  val width  = intersections(0).length
  val height = intersections.length

  /**
   * Constructs an empty rectangular board with the given dimensions.
   */
  def this(sizeX: Int, sizeY: Int) {
    this(
      Board.emptyIntersections(sizeX, sizeY),
      Map(Black -> 0, White -> 0),
      Black,
      None)
  }

  /**
   * Constructs an empty square board with the given width and height.
   */
  def this(sideLength: Int) {
    this(sideLength, sideLength)
  }

  private def addCaptureForColor(c: Color, capture: Int): Board = {
    copy(capturesForColors = capturesForColors + (c -> (capturesForColors(c) + capture)))
  }

  def allCoords: Seq[Coords] = for (x <- 1 to width; y <- 1 to height) yield Coords(x, y)

  def apply(p: Play): Board = play(p)

  /**
   * @return Whether the coordinates are legal.
   */
  def canGet(pos: Coords): Boolean = (pos.x >= 1 && pos.y >= 1 && pos.x <= width && pos.y <= height)

  def canPlay(c: Color, x: Int, y: Int): Boolean = canPlay(c to (x, y))

  /**
   * Checks that it is the right color's turn, that the intersection is empty,
   * that the stone is left alive after the move and that the intersection is not under ko.
   */
  def canPlay(move: Move): Boolean = {
    // The correct color must be in turn.
    whoseTurn == move.color &&
    // The intersection must be empty.
    get(move.pos).isEmpty &&
    // A stone at the intersection must be alive after removing the groups it captures.
    replace(Some(move.color), move.pos).removeCapturedGroups(move.color.invert).isAlive(move.pos) &&
    // The intersection must not be under ko.
    !(koPosition.isDefined && koPosition.get == move.pos)
  }

  /**
   * @return A group graph (intersections as keys, neighbors as values) for the stone group or
   * connected empty territory at the given position.
   */
  private def componentAt(pos: Coords): Set[Coords] = {
    require(canGet(pos))

    /**
     * @return A graph of all stones of the given color (or empty intersections if given None).
     * Coordinates as keys, neighbor coordinates as values.
     */
    def boardGraph(intersectionType: Option[Color]): Map[Coords, Set[Coords]] = {
      val neighborsForCoords: Seq[(Coords, Set[Coords])] = {
        for (pos <- allCoords.filter(get(_) == intersectionType)) yield {
          pos -> neighborCoords(pos).filter(get(_) == intersectionType).toSet
        }
      }
      neighborsForCoords.toMap
    }

    /**
     * @return Group or territory members accumulated with breadth-first search.
     */
    @tailrec
    def buildComponent(graph:     Map[Coords, Set[Coords]],
                       toVisit:   Seq[Coords],
                       visited:   Set[Coords],
                       component: Seq[Coords]): Seq[Coords] = {

      if (toVisit.nonEmpty) {
        val currentCoords = toVisit.head
        val unvisitedNeighbors = (graph(currentCoords) -- visited -- toVisit).toSeq
        // Accumulate component with recursion (recursive call with currentCoords prepended to component).
        buildComponent(graph, toVisit.tail ++ unvisitedNeighbors, visited + currentCoords, component :+ currentCoords)
      }
      else component
    }

    buildComponent(boardGraph(get(pos)), Seq(pos), Set.empty, Seq.empty).toSet
  }

  private def components(intersectionType: Option[Color]): Set[Set[Coords]] = {
    (for (pos <- allCoords.filter(get(_) == intersectionType)) yield componentAt(pos)).toSet
  }

  /**
   * Can be used to pass a turn.
   *
   * @return Board with turn color inverted and ko reset.
   */
  private def endTurn: Board = copy(whoseTurn = whoseTurn.invert, koPosition = None)

  def get(x: Int, y: Int): Option[Color] = get(Coords(x, y))

  /**
   * @return The intersection at the given position. None indicates an empty intersection.
   */
  def get(pos: Coords): Option[Color] = {
    require(canGet(pos))
    intersections(pos.y - 1)(pos.x - 1)
  }

  /**
   * @return The board graph component for the stone group at the given position.
   */
  def groupAt(pos: Coords): Set[Coords] = {
    require(get(pos).isDefined)
    componentAt(pos)
  }

  /**
   * @return All groups of the given color. Groups are sets of connected coordinates with stones of the same color.
   */
  def groups(c: Color): Set[Set[Coords]] = {
    components(Some(c))
  }

  /**
   * @return The set of hoshi (star point) positions on this board.
   */
  def hoshi: Set[Coords] = {
    // Center of the board.
    val tengen = Coords(width / 2 + 1, height / 2 + 1)
    // Smallest dimension of a rectangular board.
    val smallestBoardDimension: Int = Seq(width, height).min
    // Distance from the nearest board edge to corner and side hoshis is square root of the smallest board dimension.
    val fromEdgeToHoshi: Int = Math.sqrt(smallestBoardDimension).round.toInt

    def cornerHoshi(distanceToEdge: Int): Set[Coords] = {
      require(distanceToEdge > 0)

      // Possible coordinate values for corner hoshi: near the coordinates origin and on the other end.
      val xValues = Set(distanceToEdge, width  - distanceToEdge + 1)
      val yValues = Set(distanceToEdge, height - distanceToEdge + 1)

      // Build all possible (x, y) coordinate combinations from the above values.
      (for (x <- xValues; y <- yValues) yield Coords(x, y)).toSet
    }

    def sideHoshi(distanceToEdge: Int): Set[Coords] = {
      require(distanceToEdge > 0)
      Set(Coords(tengen.x, distanceToEdge), Coords(tengen.x, height - distanceToEdge + 1),
          Coords(distanceToEdge, tengen.y), Coords(width - distanceToEdge + 1, tengen.y))
    }

    // When to add which hoshi type: tengen at 5x5, corners at 9x9, sides at 17x17.
    // Hoshi are defined only for boards with uneven width and height.
    if (Set(width, height).forall(_ % 2 == 1)) {

      smallestBoardDimension match {
        // Corners, sides and tengen for large boards.
        case l if l >= 17
          => cornerHoshi(fromEdgeToHoshi) ++ sideHoshi(fromEdgeToHoshi) + tengen
        // Corners and tengen for medium boards.
        case m if m >= 9
          => cornerHoshi(fromEdgeToHoshi) + tengen
        // Only tengen for small boards.
        case s if s >= 5
          => Set(tengen)
        // None for very small boards.
        case _
          => Set.empty
      }
    }
    else Set.empty
  }

  /**
   * @return The amount of empty intersections surrounding the given position.
   */
  def liberties(pos: Coords): Int = neighbors(pos) count (_ isEmpty)

  /**
   * @return The coordinates for all neighbor intersections of the given position.
   */
  def neighborCoords(pos: Coords): Seq[Coords] = {
    // Coordinate differences for neighboring intersections in four directions: left, right, up, down.
    Seq(Coords(-1, 0), Coords(1, 0), Coords(0, -1), Coords(0, 1))
      .map   (diff => Coords(pos.x + diff.x, pos.y + diff.y)) // Map to actual coordinates
      .filter(canGet _)                                      // Filter valid coordinates
  }

  /**
   * @return All neighboring intersections for the given position – may contain empty intersections.
   */
  def neighbors(pos: Coords): Seq[Option[Color]] = {
    require(canGet(pos))
    neighborCoords(pos).map(get _)
  }

  /**
   * @return Coordinates for all neighbor stones of the given position with the given color.
   */
  def neighborStoneCoords(pos: Coords, c: Color): Seq[Coords] = {
    require(canGet(pos))
    neighborCoords(pos).filter(get(_).isDefined).filter(get(_).get == c)
  }

  /**
   * @return How many stones of the given color are on the board.
   */
  def numStones(c: Color): Int = {
    allCoords.filter(get(_).isDefined).count(get(_).get == c)
  }

  def play(x: Int, y: Int): Board = play(Coords(x, y))
  def play(pos: Coords):    Board = play(whoseTurn to pos)

  /**
   * Handles plays (moves and passes).
   * If placement is possible, returns a clone of the board with a new stone at given position and with the other player in turn.
   * If placement is not possible, returns a clone of the current board without ending the turn.
   */
  def play(p: Play): Board = {
    p match {
      case m: Move => playMove(m)
      case p: Pass => endTurn
    }
  }

  def playMove(move: Move): Board = {
    if (!canPlay(move)) {
      copy()
    }
    else {
      // Put the stone in place, remove captured groups and invert turn
      val boardForNextTurn = replace(Some(whoseTurn), move.pos).removeCapturedGroups(whoseTurn invert).endTurn

      // Mark ko position if needed.

      // If the move captures exactly one stone
      if (boardForNextTurn.capturesForColors(whoseTurn) == capturesForColors(whoseTurn) + 1) {

        // See what will be captured by this move: get the first stone of the intersection of the play coordinates'
        // neighborhood before and after this move
        val neighborsNow  = neighborStoneCoords(move.pos, whoseTurn.invert)
        val neighborsNext = boardForNextTurn.neighborStoneCoords(move.pos, whoseTurn.invert)
        val singleCapturedPosition = neighborsNow.diff(neighborsNext)(0)

        // If the opponent could play back at the captured position to capture exactly one stone (if ko was not set)
        if (boardForNextTurn.replace(Some(boardForNextTurn.whoseTurn), singleCapturedPosition).removeCapturedGroups(boardForNextTurn.whoseTurn).capturesForColors(boardForNextTurn.whoseTurn) == capturesForColors(boardForNextTurn.whoseTurn) + 1) {
          // Mark ko position
          boardForNextTurn.copy(koPosition = Option(singleCapturedPosition))
        }
        else boardForNextTurn
      }
      else boardForNextTurn
    }
  }

  /**
   * @return Board with all captured groups of the given color removed.
   */
  private def removeCapturedGroups(c: Color): Board = {
    var operatedBoard = copy()
    var capturedCount = 0

    // For all groups that have zero liberties for every stone
    for (deadGroup <- groups(c).filter(_.nonEmpty).filter(_.forall(liberties(_) == 0))) yield {
      // Remove all the group's stones
      deadGroup.foreach { stone =>
        operatedBoard = operatedBoard.replace(None, stone)
      }
      capturedCount += deadGroup.size
    }
    operatedBoard.addCaptureForColor(whoseTurn, capturedCount)
  }

  /**
   * Replaces or removes a stone at the given position without ending the turn.
   */
  private def replace(intersection: Option[Color], pos: Coords): Board = {
    val newRow = intersections(pos.y - 1)
    val newIntersections = intersections.updated(pos.y - 1, newRow.updated(pos.x - 1, intersection))
    copy(intersections = newIntersections)
  }

  /**
   * @return All encircled territories that have only stones of the given color as neighbors.
   */
  def territories(c: Color): Set[Set[Coords]] = {
    components(None).filter(_.forall(neighborStoneCoords(_, c.invert).isEmpty))
  }

  /**
   * @return The board graph component for the empty territory at the given position.
   */
  def territoryAt(pos: Coords): Set[Coords] = {
    require(!get(pos).isDefined)
    componentAt(pos)
  }

  override def toString: String = {
    intersections.foldLeft("") {
      (boardString, row) => boardString + row.foldLeft("") {
        (rowString, intersection) => rowString + (if (intersection.isEmpty) "+" else intersection.get)
      } + "\n"
    }
  }

  /**
   * @return Whether the group at pos has any liberties.
   */
  def isAlive(pos: Coords): Boolean = {
    groupAt(pos).exists(liberties(_) > 0)
  }

}

object Board {

  def emptyIntersections(width: Int, height: Int): Seq[Seq[Option[Color]]] = {
    require(width > 1 && height > 1)
    Vector.fill(height, width)(None)
  }

}