package com.github.joonasrouhiainen.joogo

case class Board(val intersections: IndexedSeq[IndexedSeq[Option[Color]]], val whoseTurn: Color) {

  def this(sizeX: Int, sizeY: Int) {
    this(Vector.fill(sizeY, sizeX)(None), Black)
  }

  val sizeX  = intersections(0) length
  val sizeY  = intersections length

  def canPlace(c: Color, x: Int, y: Int): Boolean = {
    require(x >= 1 && y >= 1 && x <= sizeX && y <= sizeY)

    whoseTurn == c && intersections(y - 1)(x - 1).isEmpty
  }

  def get(x: Int, y: Int): Option[Color] = intersections(y - 1)(x - 1)

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