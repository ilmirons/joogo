package com.github.joonasrouhiainen.joogo

import scala._
import scala.annotation.tailrec

/**
 * Represents a rectangular go board with a minimum size of 1x1.
 *
 * @author Joonas Rouhiainen
 */
case class Board(intersections:     Seq[Seq[Option[Color]]],
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

  private def addCaptureForColor(col: Color, capture: Int): Board = {
    copy(capturesForColors = capturesForColors + (col -> (capturesForColors(col) + capture)))
  }

  def allCoords: Seq[Coords] = Coords.all(width, height)

  /**
   * Checks whether the coordinates are legal.
   */
  def canGet(pos: Coords): Boolean = (pos.x >= 1 && pos.y >= 1 && pos.x <= width && pos.y <= height)

  def canPlay(col: Color, x: Int, y: Int): Boolean = canPlay(col, Coords(x, y))

  /**
   * Checks that it is the right color's turn and that the intersection is empty.
   */
  def canPlay(col: Color, pos: Coords): Boolean = {
    // The correct color must be in turn.
    whoseTurn == col &&
    // The intersection must be empty.
    get(pos).isEmpty &&
    // The intersection either has some liberties or all neighbors have the same color and it's not the last liberty of the group or playing there captures something.
    (liberties(pos) > 0 || neighbors(pos).forall(_.get == whoseTurn) && !wouldCapture(col.invert, pos) || wouldCapture(col, pos)) &&
    // The intersection is not under ko.
    !(koPosition.isDefined && koPosition.get == pos)
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
   * Returns a group graph (intersections as keys, neighbors as values) for the group at given position.
   */
  def groupAt(pos: Coords): Set[Coords] = {
    require(canGet(pos))

    /**
     * Builds a graph of all stones of the given color: stone coordinates as keys, neighbor stone coordinates as values.
     */
    def stoneGraph(c: Color): Map[Coords, Set[Coords]] = {
      var boardGraph = Map[Coords, Set[Coords]]()

      allCoords.map {
        pos => {
          if (get(pos).isDefined && get(pos).get == c) {
            boardGraph = boardGraph + (pos -> neighborStoneCoords(pos, c).toSet)
          }
        }
      }
      boardGraph
    }

    /**
     * Accumulates group members with breadth-first search.
     */
    @tailrec
    def buildGroup(graph:        Map[Coords, Set[Coords]],
                   toVisit:      Seq[Coords],
                   visited:      Set[Coords],
                   groupMembers: Seq[Coords]): Seq[Coords] = {

      if (toVisit.nonEmpty) {
        val currentCoords = toVisit.head
        val unvisitedNeighbors = (graph(currentCoords) -- visited -- toVisit).toSeq
        // Accumulate groupMembers with recursion (recursive call with currentCoords prepended to groupMembers).
        buildGroup(graph, toVisit.tail ++ unvisitedNeighbors, visited + currentCoords, groupMembers :+ currentCoords)
      }
      else groupMembers
    }

    if (get(pos).isDefined) {
      buildGroup(stoneGraph(get(pos).get), Seq(pos), Set.empty, Seq.empty).toSet
    }
    else Set.empty
  }

  /**
   * Returns all groups of the given color. Groups are sets of connected coordinates with stones of the same color.
   */
  def groups(c: Color): Set[Set[Coords]] = {
    var groups = Set[Set[Coords]]()

    allCoords.map {
      pos => {
        if (get(pos).isDefined && get(pos).get == c) {
          groups = groups + groupAt(pos)
        }
      }
    }
    groups
  }

  def liberties(pos: Coords): Int = neighbors(pos) count (_ isEmpty)

  /**
   * Returns the coordinates for all neighbor intersections of the given position.
   */
  def neighborCoords(pos: Coords): Seq[Coords] = {
    Seq(Coords(-1, 0), Coords(1, 0), Coords(0, -1), Coords(0, 1)) // Coordinate differences for neighboring intersections in four directions: left, right, up, down.
      .map   (diff => Coords(pos.x + diff.x, pos.y + diff.y))     // Map to actual coordinates
      .filter(canGet(_))                                          // Filter valid coordinates
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

  override def toString: String = {
    intersections.foldLeft("") {
      (boardString, row) => boardString + row.foldLeft("") {
        (rowString, intersection) => rowString + (if (intersection.isEmpty) "+" else intersection.get)
      } + "\n"
    }
  }

  /**
   * Returns true if playing the given color to the position would result in a capture.
   */
  def wouldCapture(c: Color, pos: Coords): Boolean = {
    replace(Some(c), pos).intersections != replace(Some(c), pos).removeCapturedGroups(c.invert).intersections
  }

}

object Board {

  def emptyIntersections(width: Int, height: Int): Seq[Seq[Option[Color]]] = {
    require(width > 0 && height > 0)
    Vector.fill(height, width)(None)
  }

}