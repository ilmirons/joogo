package com.github.joonasrouhiainen.joogo

import scala._
import scala.Some

/**
 * Represents a rectangular go board with a minimum size of 1x1.
 */
case class Board(val intersections: IndexedSeq[IndexedSeq[Option[Color]]], val whoseTurn: Color) {

  val sizeX  = intersections(0) length
  val sizeY  = intersections length

  /**
   * Constructs an empty board with the given size.
   */
  def this(sizeX: Int, sizeY: Int) {
    this(Board.emptyIntersections(sizeX, sizeY), Black)
  }

  /**
   * Checks if a stone of the given color can be placed at (x, y).
   */
  def canPlace(c: Color, x: Int, y: Int): Boolean = {
    require(x >= 1 && y >= 1 && x <= sizeX && y <= sizeY)

    whoseTurn == c && intersections(y - 1)(x - 1).isEmpty
  }

  /**
   * Gets the stone at given position.
   */
  def get(x: Int, y: Int): Option[Color] = intersections(y - 1)(x - 1)

  /**
   * If placement is possible, returns a clone of the board with a new stone at given position.
   * If placement is not possible, returns a clone of the current board.
   */
  def place(c: Color, x: Int, y: Int): Board = {
    if (!canPlace(c, x, y)) {
      new Board(intersections, whoseTurn)
    }
    else {
      val newRow = intersections(y - 1)
      val newIntersections = intersections.updated(y - 1, newRow.updated(x - 1, Some(c)))
      new Board(newIntersections, c invert)
    }
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