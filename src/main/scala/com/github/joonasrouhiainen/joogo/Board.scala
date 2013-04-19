package com.github.joonasrouhiainen.joogo

class Board(val intersections: Array[Array[Option[Color]]]) {

  def this(sizeX: Int, sizeY: Int) {
    this(Array.ofDim[Option[Color]](sizeY, sizeX).map(_.map(_ => None.asInstanceOf[Option[Color]])))
  }

  val isFull = false
  val sizeX  = intersections(0) length
  val sizeY  = intersections length

  def canPlace(x: Int, y: Int): Boolean = {
    require(x >= 1 && y >= 1 && x <= sizeX && y <= sizeY)

    intersections(y - 1)(x - 1) isEmpty
  }

  def get(x: Int, y: Int): Option[Color] = intersections(y)(x)

  def place(c: Color, x: Int, y: Int): Board = {
    if (!canPlace(x, y)) {
      new Board(intersections clone)
    }
    else {
      new Board(intersections clone)
    }
  }

  override def toString(): String = {
    intersections.foldLeft("") {
      (boardString, row) => boardString + row.foldLeft("") {
        (rowString, intersection) => rowString + (if (intersection isEmpty) "+" else intersection.get)
      } + "\n"
    }
  }

  def whoseTurn(): Color = Black

}