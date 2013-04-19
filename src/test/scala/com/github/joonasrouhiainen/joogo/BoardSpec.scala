package com.github.joonasrouhiainen.joogo

import org.specs2.mutable._

class BoardSpec extends Specification {

  var board: Board = _

  step {
    board = new Board()
  }

  "A new board" should {

    "be empty when created" in {
      board.toString must_==
        "+++++++++\n" +
        "+++++++++\n" +
        "+++++++++\n" +
        "+++++++++\n" +
        "+++++++++\n" +
        "+++++++++\n" +
        "+++++++++\n" +
        "+++++++++\n" +
        "+++++++++\n"
    }

    "allow placing stones" in {
      board.isFull must beFalse
    }

    "allow placing a stone at any intersection" in {
      (1 to board.size) foreach {
        i => (1 to board.size) foreach {
          j => board.canPlace(i, j) must beTrue
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

}
