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

  def allCoords: Seq[Coords] = Coords.all(width, height)

  /**
   * Checks whether the coordinates are legal.
   */
  def canGet(pos: Coords): Boolean = (pos.x >= 1 && pos.y >= 1 && pos.x <= width && pos.y <= height)

  def canPlay(c: Color, x: Int, y: Int): Boolean = canPlay(c, Coords(x, y))

  /**
   * Checks that it is the right color's turn and that the intersection is empty.
   */
  def canPlay(c: Color, pos: Coords): Boolean = {
    // The correct color must be in turn.
    whoseTurn == c &&
    // The intersection must be empty.
    get(pos).isEmpty &&
    // A stone at the intersection must be alive after removing the groups it captures.
    replace(Some(c), pos).removeCapturedGroups(c.invert).isAlive(pos) &&
    // The intersection must not be under ko.
    !(koPosition.isDefined && koPosition.get == pos)
  }

  /**
   * Returns a group graph (intersections as keys, neighbors as values) for the stone group or
   * connected empty territory at the given position.
   */
  private def componentAt(pos: Coords): Set[Coords] = {
    require(canGet(pos))

    /**
     * Builds a graph of all stones of the given color (or empty intersections if given None).
     * Coordinates as keys, neighbor coordinates as values.
     */
    def boardGraph(intersectionType: Option[Color]): Map[Coords, Set[Coords]] = {
      var graph = Map[Coords, Set[Coords]]()

      allCoords.map {
        pos => {
          if (get(pos) == intersectionType) {
            graph = graph + (pos -> neighborCoords(pos).filter(get(_) == intersectionType).toSet)
          }
        }
      }
      graph
    }

    /**
     * Accumulates group or territory members with breadth-first search.
     */
    @tailrec
    def buildComponent(graph:        Map[Coords, Set[Coords]],
                       toVisit:      Seq[Coords],
                       visited:      Set[Coords],
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
    var foundComponents = Set[Set[Coords]]()

    allCoords.map {
      pos => {
        if (get(pos) == intersectionType) {
          foundComponents = foundComponents + componentAt(pos)
        }
      }
    }
    foundComponents
  }

  /**
   * Inverts color in turn and resets ko. Can be used to pass a turn.
   */
  def endTurn: Board = copy(whoseTurn = whoseTurn.invert, koPosition = None)

  def get(x: Int, y: Int): Option[Color] = get(Coords(x, y))

  /**
   * Gets the intersection at the given position.
   */
  def get(pos: Coords): Option[Color] = {
    require(canGet(pos))
    intersections(pos.y - 1)(pos.x - 1)
  }

  /**
   * Returns the board graph component for the stone group at the given position.
   */
  def groupAt(pos: Coords): Set[Coords] = {
    require(get(pos).isDefined)
    componentAt(pos)
  }

  /**
   * Returns all groups of the given color. Groups are sets of connected coordinates with stones of the same color.
   */
  def groups(c: Color): Set[Set[Coords]] = {
    components(Some(c))
  }

  def hoshi: Set[Coords] = {
    // Define hoshi for square boards only
    if (width == height) {
      val centerCoord = width / 2 + 1
      val tengen = Coords(centerCoord, centerCoord)

      width match {
        case 9  => Set(Coords(3, 3),  Coords(7, 3),   Coords(3, 7),  Coords(7, 7),   tengen)
        case 13 => Set(Coords(4, 4),  Coords(10, 4),  Coords(4, 10), Coords(10, 10), tengen)
        case 19 => Set(Coords(4, 4),  Coords(10, 4),  Coords(16, 4),
                       Coords(4, 10), Coords(10, 10), Coords(16, 10),
                       Coords(4, 16), Coords(10, 16), Coords(16, 16))
        case _ => Set.empty
      }
    }
    else Set.empty
  }

  def liberties(pos: Coords): Int = neighbors(pos) count (_ isEmpty)

  /**
   * Returns the coordinates for all neighbor intersections of the given position.
   */
  def neighborCoords(pos: Coords): Seq[Coords] = {
    // Coordinate differences for neighboring intersections in four directions: left, right, up, down.
    Seq(Coords(-1, 0), Coords(1, 0), Coords(0, -1), Coords(0, 1))
      .map   (diff => Coords(pos.x + diff.x, pos.y + diff.y)) // Map to actual coordinates
      .filter(canGet _)                                      // Filter valid coordinates
  }

  /**
   * Returns all neighboring intersections for the given position – may contain empty intersections.
   */
  def neighbors(pos: Coords): Seq[Option[Color]] = {
    require(canGet(pos))
    neighborCoords(pos).map(get _)
  }

  /**
   * Returns the coordinates for all neighbor stones of the given position with the given color.
   */
  def neighborStoneCoords(pos: Coords, c: Color): Seq[Coords] = {
    require(canGet(pos))
    neighborCoords(pos).filter(get(_).isDefined).filter(get(_).get == c)
  }

  /**
   * Counts how many stones of the given color are on the board.
   */
  def numStones(c: Color): Int = {
    allCoords.filter(get(_).isDefined).count(get(_).get == c)
  }

  def play(x: Int, y: Int): Board = play(Coords(x, y))

  /**
   * If placement is possible, returns a clone of the board with a new stone at given position and with the other player in turn.
   * If placement is not possible, returns a clone of the current board without ending the turn.
   */
  def play(pos: Coords): Board = {
    if (!canPlay(whoseTurn, pos)) {
      copy()
    }
    else {
      // Put the stone in place, remove captured groups and invert turn
      val boardForNextTurn = replace(Some(whoseTurn), pos).removeCapturedGroups(whoseTurn invert).endTurn

      // Mark ko position if needed.

      // If the move captures exactly one stone
      if (boardForNextTurn.capturesForColors(whoseTurn) == capturesForColors(whoseTurn) + 1) {

        // See what will be captured by this move: get the first stone of the intersection of the play coordinates'
        // neighborhood before and after this move
        val singleCapturedPosition = neighborStoneCoords(pos, whoseTurn.invert).diff(boardForNextTurn.neighborStoneCoords(pos, whoseTurn.invert))(0)

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
   * Removes all captured groups of the given color.
   */
  private def removeCapturedGroups(c: Color): Board = {
    var operatedBoard = copy()
    var capturedCount = 0

    groups(c).foreach {
      group => {
        if (group.nonEmpty && group.forall(liberties(_) == 0)) {
          // If all members have zero liberties, remove the group's stones
          group.foreach {
            pos => {
              operatedBoard = operatedBoard.replace(None, pos)
              capturedCount += 1
            }
          }
        }
      }
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
   * Returns all encircled territories that have only stones of the given color as neighbors.
   */
  def territories(c: Color): Set[Set[Coords]] = {
    components(None).filter(_.forall(neighborStoneCoords(_, c.invert).isEmpty))
  }

  /**
   * Returns the board graph component for the empty territory at the given position.
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
   * Tests whether the group at pos has any liberties.
   */
  def isAlive(pos: Coords): Boolean = {
    groupAt(pos).exists(liberties(_) > 0)
  }

}

object Board {

  def emptyIntersections(width: Int, height: Int): Seq[Seq[Option[Color]]] = {
    require(width > 0 && height > 0)
    Vector.fill(height, width)(None)
  }

}