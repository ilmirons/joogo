package com.github.joonasrouhiainen.joogo

import org.specs2.mutable._

class BoardSpec extends Specification {

  var board: Board = _

  step {
    board = new Board(9, 7)
  }

  "A new 9x7 board" should {

    "be empty when created" in {
      board.toString must_== "+++++++++\n" * 7
    }

    "have a sizeX of 9" in {
      board.sizeX must_== 9
    }

    "have a sizeY of 7" in {
      board.sizeY must_== 7
    }

    "allow placing a stone at any intersection" in {
      (1 to board.sizeX) foreach {
        x => (1 to board.sizeY) foreach {
          y => board.canPlace(x, y) must beTrue
        }
      }
    }

    "disallow placing a stone at (0,0)" in {
      board.canPlace(0, 0) must throwA [IllegalArgumentException]
    }

    "have black next in turn" in {
      board.whoseTurn must_== Black
    }

  }

  "Placing a black stone on an empty board" should {

    "make the stone retrievable" in {
      board.place(Black, 1, 2).get(1, 2).get must_== Black
    }

    "make the stone appear at the right place" in {
      board.place(Black, 1, 2).toString must_==
      "+++++++++\n" +
      "b++++++++\n" +
      "+++++++++\n" +
      "+++++++++\n" +
      "+++++++++\n" +
      "+++++++++\n" +
      "+++++++++\n"
    }

    "leave the original board state untouched" in {
      board.place(Black, 1, 2)
      board.get(1, 2).isEmpty must beTrue
    }

    "make it white's turn" in {
      board.place(Black, 1, 2).whoseTurn must_== White
    }

  }

}
