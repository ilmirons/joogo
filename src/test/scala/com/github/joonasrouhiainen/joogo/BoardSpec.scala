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
    val cornerCoords  = Set((1, 1), (1, board.height), (board.width, 1), (board.width, board.height))
    val illegalCoords = Set((0, 0), (-1, -1), (board.height + 1, board.height + 1))

    "be empty when created" in {
      board.toString must_== "+++++++++\n" * 7
    }

    "have a width of 9" in {
      board.width must_== 9
    }

    "have a height of 7" in {
      board.height must_== 7
    }

    "have black and white captured count of 0" in {
      board.capturesForColors(Black) must_== 0
      board.capturesForColors(Black) must_== board.capturesForColors(White)
    }

    "allow playing a black stone at any intersection" in {
      (1 to board.width) foreach {
        x => (1 to board.height) foreach {
          y => board.canPlay(Black, x, y) must beTrue
        }
      }
    }

    illegalCoords.foreach {
      case (x: Int, y: Int) => {

        val pos = "(" + x + ", " + y + ")"

        "disallow playing a black stone at " + pos in {
          board.canPlay(Black, x, y) must throwA [IllegalArgumentException]
        }

        "disallow getting a stone at " + pos in {
          board.get(x, y) must throwA [IllegalArgumentException]
        }

      }
    }

    "have black next in turn" in {
      board.whoseTurn must_== Black
    }

    "have white next in turn after a pass" in {
      board.endTurn.whoseTurn must_== White
    }

    "have only empty neighbors for every intersection" in {
      (1 to board.width) foreach {
        x => (1 to board.height) foreach {
          y => board.neighbors(x, y).foreach(_.isEmpty must beTrue)
        }
      }
    }

    "have a neighbor count of 4 for all intersections not near the edges" in {
      (2 until board.width) foreach {
        x => (2 until board.height) foreach {
          y => board.neighbors(x, y).size must_== 4
        }
      }
    }

    "have a neighbor count of 2 for intersections in the four corners" in {
      cornerCoords.foreach {
        case (x: Int, y: Int) => {
          board.neighbors(x, y).size must_== 2
        }
      }
    }

    "have a neighbor count of 3 for intersections near the edges but not in corners" in {

      val mustHaveThreeNeighbors = (x: Int, y: Int) => if (!cornerCoords.contains((x, y))) board.neighbors(x, y).size must_== 3

      // First row and last row
      List(1, board.height).foreach { y =>
        (2 until board.width).foreach(x => mustHaveThreeNeighbors(x, y))
      }
      // First col and last col
      List(1, board.width).foreach{ x =>
        (2 until board.height).foreach(y => mustHaveThreeNeighbors(x, y))
      }
    }

  }

  "Playing the first stone on an empty board" should {

    val emptyBoard = new Board(9, 7)
    val board      = emptyBoard.play(1, 2)
    val neighbors  = Set((1, 1), (2, 2), (1,3))

    "make a black stone retrievable from the position" in {
      board.get(1, 2).get must_== Black
    }

    "make a black stone appear at the right place" in {
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

    neighbors.foreach {
      case (x: Int, y: Int) => {

        val pos = "(" + x + ", " + y + ")"

        "make neighbor at " + pos + " have exactly one neighbor" in {
          board.neighbors(x, y).count(_.isDefined) must_== 1
        }

        "make neighbor at " + pos + " have a black neighbor" in {
          board.neighbors(x, y).find(_.isDefined).get.get must_== Black
        }
      }
    }
  }

  "Taking the last liberty from a single black stone in a corner" should {

    val board = new Board(9, 7).play(1, 1).play(1, 2).play(9, 7).play(2, 1)

    "remove that stone from the board" in {
      board.toString must_==
      "+w+++++++\n" +
      "w++++++++\n" +
      "+++++++++\n" +
      "+++++++++\n" +
      "+++++++++\n" +
      "+++++++++\n" +
      "++++++++b\n"
    }

    "make white player's captured count 1" in {
      board.capturesForColors(White) must_== 1
    }

  }
}
