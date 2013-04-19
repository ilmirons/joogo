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

    "allow placing stones" in {
      board.isFull must beFalse
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

    "contain a stone after placing it" in {
      val afterFirst = board.place(Black, 1, 2)
      afterFirst.get(1, 2) must_== Black
    }

  }

}
