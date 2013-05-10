package com.github.joonasrouhiainen.joogo

import scala._

/**
 * Represents a rectangular go board with a minimum size of 1x1.
 *
 * @author Joonas Rouhiainen
 */
case class Board(val intersections:     IndexedSeq[IndexedSeq[Option[Color]]],
                 val capturesForColors: Map[Color, Int],
                 val whoseTurn:         Color) {

  val width  = intersections(0) length
  val height = intersections length

  /**
   * Constructs an empty rectangular board with the given dimensions.
   */
  def this(sizeX: Int, sizeY: Int) {
    this(Board.emptyIntersections(sizeX, sizeY), Map(Black -> 0, White -> 0), Black)
  }

  /**
   * Constructs an empty square board with the given width and height.
   */
  def this(sideLength: Int) {
    this(sideLength, sideLength)
  }

  private def addCaptureForColor(c: Color, capture: Int): Board = {
    new Board(intersections,
              capturesForColors + (c -> (capturesForColors(c) + capture)),
              whoseTurn)
  }

  /**
   * Checks whether the coordinates are legal.
   */
  def canGet(x: Int, y: Int) = (x >= 1 && y >= 1 && x <= width && y <= height)

  /**
   * Checks that it is the right color's turn and that the intersection is empty.
   */
  def canPlay(c: Color, x: Int, y: Int): Boolean = {
    require(canGet(x, y))

    // The correct color must be in turn.
    whoseTurn == c &&
    // The intersection must be empty.
    intersections(y - 1)(x - 1).isEmpty &&
    // The intersection can't have zero liberties and be surrounded by opponent's stones.
    !(liberties(x, y) == 0 && neighbors(x, y).forall(_.get == whoseTurn.invert))
  }

  def endTurn(): Board = new Board(intersections, capturesForColors, whoseTurn invert)

  /**
   * Gets the intersection at the given position.
   */
  def get(x: Int, y: Int): Option[Color] = {
    require(canGet(x, y))
    intersections(y - 1)(x - 1)
  }

  def liberties(x: Int, y: Int): Int = neighbors(x, y) count (_ isEmpty)

  /**
   * Returns the coordinates for all neighbor intersections of the given position.
   */
  def neighborCoords(x: Int, y: Int): Vector[(Int, Int)] = {
    Vector((-1, 0), (1, 0), (0, -1), (0, 1)) // Coordinate differences for neighboring intersections in four directions: left, right, up, down.
      .map   { case (dx: Int, dy: Int) => (x + dx, y + dy) } // Map to actual coordinates
      .filter{ case (x:  Int, y:  Int) => canGet(x, y)     } // Filter valid coordinates
  }

  /**
   * Returns the coordinates for all neighbor stones of the given position with the given color.
   */
  def neighborStoneCoords(x: Int, y: Int, c: Color): Vector[(Int, Int)] = {
    neighborCoords(x, y) filter { case (x: Int, y: Int) => get(x, y).isDefined && get(x, y).get == c }
  }

  /**
   * Returns all neighboring intersections for the given position – may contain empty intersections.
   */
  def neighbors(x: Int, y: Int): Vector[Option[Color]] = {
    neighborCoords(x, y) map { case (x: Int, y: Int) => get(x, y) }
  }

  /**
   * If placement is possible, returns a clone of the board with a new stone at given position and with the other player in turn.
   * If placement is not possible, returns a clone of the current board without ending the turn.
   */
  def play(x: Int, y: Int): Board = {
    if (!canPlay(whoseTurn, x, y)) {
      new Board(intersections, capturesForColors, whoseTurn)
    }
    else {
      replace(Some(whoseTurn), x, y).removeCapturedGroups(whoseTurn invert).endTurn
    }
  }

  private def groupGraph(c: Color): Map[(Int, Int), Set[(Int, Int)]] = {
    var boardGraph = Map[(Int, Int), Set[(Int, Int)]]()

    (1 to width) foreach {
      x => (1 to height) foreach {
        y => {
          if (get(x, y).isDefined && get(x, y).get == c) {
            boardGraph = boardGraph + ((x, y) -> neighborStoneCoords(x, y, c).toSet)
          }
        }
      }
    }
    boardGraph
  }

  private def removeCapturedGroups(c: Color): Board = {
    var operatedBoard = new Board(intersections, capturesForColors, whoseTurn)
    var capturedCount = 0

    def traverse(graph: Map[(Int, Int), Set[(Int, Int)]], toVisit: Seq[(Int, Int)], visited: Set[(Int, Int)], previousGroupMembersWithZeroLiberties: Set[(Int, Int)]): Seq[(Int, Int)] = {
      if (toVisit isEmpty) {
        println("no more intersections to visit")

        if (previousGroupMembersWithZeroLiberties nonEmpty) {
          println("have to remove stones")
          (previousGroupMembersWithZeroLiberties) foreach {
            case (x: Int, y: Int) => {
              operatedBoard = operatedBoard.replace(None, x, y)
              capturedCount += 1
            }
          }
        }
        Seq empty
      }
      else {
        val currentCoords = toVisit.head
        println("now at " + currentCoords + ", liberties " + liberties(toVisit.head._1, toVisit.head._2) + ", previousGroupMembersWithZeroLiberties " + previousGroupMembersWithZeroLiberties)

        val x = toVisit.head._1
        val y = toVisit.head._2

        val unvisitedNeighbors = (graph(currentCoords) -- visited -- toVisit).toSeq
        println("it has unvisited neighbors " + unvisitedNeighbors)

        val zeroesForNext: Set[(Int, Int)] = liberties(x, y) match {
          case 0 => previousGroupMembersWithZeroLiberties + currentCoords
          case _ => if (unvisitedNeighbors isEmpty) Set.empty else previousGroupMembersWithZeroLiberties
        }

        currentCoords +: traverse(graph, toVisit.tail ++ unvisitedNeighbors, visited + currentCoords, zeroesForNext)
      }
    }

    def traverseFrom(graph: Map[(Int, Int), Set[(Int, Int)]], initial: (Int, Int)) = traverse(graph, Seq(initial), Set.empty, Set.empty)

    val graph = groupGraph(c)

    if (graph nonEmpty) {
      println("traversing graph from " + graph.keySet.head)
      traverseFrom(graph, graph.keySet.head)
    }
    operatedBoard.addCaptureForColor(whoseTurn, capturedCount)
  }

  /**
   * Replaces or removes a stone at the given position without ending the turn.
   */
  private def replace(intersection: Option[Color], x: Int, y: Int): Board = {
    val newRow = intersections(y - 1)
    val newIntersections = intersections.updated(y - 1, newRow.updated(x - 1, intersection))
    new Board(newIntersections, capturesForColors, whoseTurn)
  }

  override def toString(): String = {
    intersections.foldLeft("") {
      (boardString, row) => boardString + row.foldLeft("") {
        (rowString, intersection) => rowString + (if (intersection isEmpty) "+" else intersection get)
      } + "\n"
    }
  }

}

object Board {

  def emptyIntersections(width: Int, height: Int): IndexedSeq[IndexedSeq[Option[Color]]] = {
    require(width > 0 && height > 0)
    Vector.fill(height, width)(None)
  }

}