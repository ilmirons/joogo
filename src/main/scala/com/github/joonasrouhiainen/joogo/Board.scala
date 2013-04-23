package com.github.joonasrouhiainen.joogo

import scala._
import scala.Some

/**
 * Represents a rectangular go board with a minimum size of 1x1.
 */
case class Board(val intersections:     IndexedSeq[IndexedSeq[Option[Color]]],
                 val capturesForColors: Map[Color, Int],
                 val whoseTurn: Color) {

  val sizeX = intersections(0) length
  val sizeY = intersections length

  /**
   * Constructs an empty board with the given size.
   */
  def this(sizeX: Int, sizeY: Int) {
    this(Board.emptyIntersections(sizeX, sizeY), Map(Black -> 0, White -> 0), Black)
  }

  private def addCaptureForColor(c: Color, capture: Int): Board = {
    new Board(intersections,
              capturesForColors + (c -> (capturesForColors(c) + capture)),
              whoseTurn)
  }

  /**
   * Checks whether the coordinates are legal.
   */
  def canGet(x: Int, y: Int) = (x >= 1 && y >= 1 && x <= sizeX && y <= sizeY)

  /**
   * Checks that it is the right color's turn and that the intersection is empty.
   */
  def canPlay(c: Color, x: Int, y: Int): Boolean = {
    require(canGet(x, y))
    whoseTurn == c && intersections(y - 1)(x - 1).isEmpty
  }

  def endTurn(): Board = new Board(intersections, capturesForColors, whoseTurn invert)

  /**
   * Gets the stone at given position.
   */
  def get(x: Int, y: Int): Option[Color] = {
    require(canGet(x, y))
    intersections(y - 1)(x - 1)
  }

  /**
   * Returns all neighboring intersections for the given position.
   */
  def neighbors(x: Int, y: Int): Vector[Option[Color]] = {

    // Coordinate differences for neighboring intersections in four directions: left, right, up, down.
    val neighborCoordDiffs = Vector((-1, 0), (1, 0), (0, -1), (0, 1))

    // Filter all legal differences for this position and map them to actual intersections
    neighborCoordDiffs.filter({ case (dx: Int, dy: Int) => canGet(x + dx, y + dy)})
                      .map   ({ case (dx: Int, dy: Int) =>    get(x + dx, y + dy)})
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
      replace(Some(whoseTurn), x, y).removeCaptured.endTurn
    }
  }

  private def removeCaptured(): Board = {
    var operatedBoard = new Board(intersections, capturesForColors, whoseTurn)
    var capturedCount = 0

    (1 to sizeX).foreach {
      x => (1 to sizeY).foreach {
        y => {
          val pos = get(x, y)

          if (pos isDefined) {
            val neighboring: Vector[Option[Color]] = neighbors(x, y)

            if (neighboring.forall(_.isDefined) && neighboring.forall(_.get == whoseTurn)) {
              operatedBoard = replace(None, x, y)
              capturedCount += 1
            }
          }
        }
      }
    }
    operatedBoard.addCaptureForColor(whoseTurn, capturedCount)
  }

  /**
   * Replaces or removes a stone at given position without ending the turn.
   */
  private def replace(intersection: Option[Color], x: Int, y: Int): Board = {
    val newRow = intersections(y - 1)
    val newIntersections = intersections.updated(y - 1, newRow.updated(x - 1, intersection))
    new Board(newIntersections, capturesForColors, whoseTurn)
  }

  override def toString(): String = {
    intersections.foldLeft("") {
      (boardString, row) => boardString + row.foldLeft("") {
        (rowString, intersection) => rowString + (if (intersection isEmpty) "+" else intersection.get)
      } + "\n"
    }
  }

}

object Board {

  def emptyIntersections(sizeX: Int, sizeY: Int): IndexedSeq[IndexedSeq[Option[Color]]] = {
    require(sizeX > 0 && sizeY > 0)
    Vector.fill(sizeY, sizeX)(None)
  }

}