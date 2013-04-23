package com.github.joonasrouhiainen.joogo

import org.specs2.mutable._

class BoardSpec extends Specification {

  "A new 0x0 board" should {

    "be disallowed" in {
      new Board(0, 0) must throwA[IllegalArgumentException]
    }

  }

  "A new 9x7 board" should {

    val board = new Board(9, 7)

    "be empty when created" in {
      board.toString must_== "+++++++++\n" * 7
    }

    "have a sizeX of 9" in {
      board.sizeX must_== 9
    }

    "have a sizeY of 7" in {
      board.sizeY must_== 7
    }

    "allow placing a black stone at any intersection" in {
      (1 to board.sizeX) foreach {
        x => (1 to board.sizeY) foreach {
          y => board.canPlace(Black, x, y) must beTrue
        }
      }
    }

    "disallow placing a black stone at (0,0)" in {
      board.canPlace(Black, 0, 0) must throwA [IllegalArgumentException]
    }

    "have black next in turn" in {
      board.whoseTurn must_== Black
    }

    "not react to placing a white stone" in {
      board.place(White, 1, 2) must_== board
    }

  }

  "Placing a black stone on an empty board" should {

    val emptyBoard = new Board(9, 7)
    val board      = emptyBoard.place(Black, 1, 2)

    "make the stone retrievable" in {
      board.get(1, 2).get must_== Black
    }

    "make the stone appear at the right place" in {
      board.toString must_==
      "+++++++++\n" +
      "b++++++++\n" +
      "+++++++++\n" +
      "+++++++++\n" +
      "+++++++++\n" +
      "+++++++++\n" +
      "+++++++++\n"
    }

    "leave the original board state untouched" in {
      emptyBoard.get(1, 2).isEmpty must beTrue
    }

    "make it white's turn" in {
      board.whoseTurn must_== White
    }

  }

}
