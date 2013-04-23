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
    val cornerCoords  = Set((1, 1), (1, board.sizeY), (board.sizeX, 1), (board.sizeX, board.sizeY))
    val illegalCoords = Set((0, 0), (-1, -1), (board.sizeY + 1, board.sizeY + 1))

    "be empty when created" in {
      board.toString must_== "+++++++++\n" * 7
    }

    "have a sizeX of 9" in {
      board.sizeX must_== 9
    }

    "have a sizeY of 7" in {
      board.sizeY must_== 7
    }

    "allow play a black stone at any intersection" in {
      (1 to board.sizeX) foreach {
        x => (1 to board.sizeY) foreach {
          y => board.canPlay(Black, x, y) must beTrue
        }
      }
    }

    illegalCoords.foreach {
      case (x: Int, y: Int) => {

        val pos = "(" + x + ", " + y + ")"

        "disallow placing a black stone at " + pos in {
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

    "have only empty neighbors for every intersection" in {
      (1 to board.sizeX) foreach {
        x => (1 to board.sizeY) foreach {
          y => board.neighbors(x, y).foreach(_.isEmpty must beTrue)
        }
      }
    }

    "have a neighbor count of 4 for all intersections not near the edges" in {
      (2 until board.sizeX) foreach {
        x => (2 until board.sizeY) foreach {
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
      List(1, board.sizeY).foreach { y =>
        (2 until board.sizeX).foreach(x => mustHaveThreeNeighbors(x, y))
      }
      // First col and last col
      List(1, board.sizeX).foreach{ x =>
        (2 until board.sizeY).foreach(y => mustHaveThreeNeighbors(x, y))
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

}
