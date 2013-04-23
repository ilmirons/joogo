package com.github.joonasrouhiainen.joogo

import org.specs2.mutable._

/**
 * Unit tests for classes Board and Color.
 *
 * @author Joonas Rouhiainen
 */
class BoardSpec extends Specification {

  "A new 0x0 board" should {

    "be disallowed" in {
      new Board(0, 0) must throwA[IllegalArgumentException]
    }

  }

  "A new square 9 board" should {

    val board = new Board(9)

    "be 9x9" in {
      board.width  must_== 9
      board.height must_== 9
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

    val board = new Board(9, 7).play(1, 1).play(1, 2).endTurn.play(2, 1)

    "make it disappear from the board string" in {
      board.toString must_==
        "+w+++++++\n" +
          "w++++++++\n" +
          "+++++++++\n" +
          "+++++++++\n" +
          "+++++++++\n" +
          "+++++++++\n" +
          "+++++++++\n"
    }

    "make it unretrievable from the position" in {
      board.get(1, 1).isEmpty must beTrue
    }

    "make white player's captured count 1" in {
      board.capturesForColors(White) must_== 1
    }
  }

  "Taking the last liberty from a single white stone in the center" should {

    val board = new Board(9, 7).play(5, 3).play(5, 4).play(5, 5).endTurn.play(4, 4).endTurn.play(6, 4)

    "make it disappear from the board string" in {
      board.toString must_==
        "+++++++++\n" +
          "+++++++++\n" +
          "++++b++++\n" +
          "+++b+b+++\n" +
          "++++b++++\n" +
          "+++++++++\n" +
          "+++++++++\n"
    }

    "make it unretrievable from the position" in {
      board.get(5, 4).isEmpty must beTrue
    }

  }

  "A board with a lonely black diamond where white tries to play inside the diamond" should {

    val board = new Board(9, 7).play(5, 3).endTurn.play(5, 5).endTurn.play(4, 4).endTurn.play(6, 4).play(5, 4)

    "not add white stones to the board string" in {
      board.toString must not contain White
    }

    "keep the diamond eye empty" in {
      board.get(5, 4).isEmpty must beTrue
    }

    "keep white still in turn" in {
      board.whoseTurn must_== White
    }

  }

}
