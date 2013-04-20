package com.github.joonasrouhiainen.joogo

class Board(val intersections: Array[Array[Option[Color]]], val whoseTurn: Color) {

  def this(sizeX: Int, sizeY: Int) {
    this(Array.ofDim[Option[Color]](sizeY, sizeX).map(_.map(_ => None.asInstanceOf[Option[Color]])), Black)
  }

  val isFull = false
  val sizeX  = intersections(0) length
  val sizeY  = intersections length

  def canPlace(x: Int, y: Int): Boolean = {
    require(x >= 1 && y >= 1 && x <= sizeX && y <= sizeY)

    intersections(y - 1)(x - 1) isEmpty
  }

  def get(x: Int, y: Int): Option[Color] = intersections(y - 1)(x - 1)

  def place(c: Color, x: Int, y: Int): Board = {
    if (!canPlace(x, y)) {
      new Board(intersections clone, whoseTurn)
    }
    else {
      val newIntersections = intersections map(_ clone)
      newIntersections(y - 1)(x - 1) = Some(c)
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