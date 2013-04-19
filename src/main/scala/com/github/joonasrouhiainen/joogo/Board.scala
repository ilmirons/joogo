package com.github.joonasrouhiainen.joogo

class Board {

  val size   = 9
  val stones = Array.ofDim[Int](size, size)
  val isFull = false

  def canPlace(x: Int, y: Int): Boolean = {
    require(x >= 1 && y >= 1 && x <= size && y <= size)
    stones(x - 1)(y - 1) == 0
  }

  override def toString(): String = {
    stones.foldLeft("") {
      (boardString, row) => boardString + row.foldLeft("") {
        (rowString, stone) => rowString + (if (stone == 0) "+" else "*")
      } + "\n"
    }
  }

}
