package com.github.joonasrouhiainen.joogo

class Board {

  val size = 9
  val intersections = Array.ofDim[Option[Color]](size, size) map(_.map(_ => None))
  val isFull = false

  def canPlace(x: Int, y: Int): Boolean = {
    require(x >= 1 && y >= 1 && x <= size && y <= size)

    intersections(x - 1)(y - 1) isEmpty
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