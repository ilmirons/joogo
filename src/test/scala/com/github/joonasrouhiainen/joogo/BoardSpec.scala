package com.github.joonasrouhiainen.joogo

import org.specs2.mutable._

/**
 * Unit tests for Board.
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
      (board.width must_== 9) and (board.height must_== 9)
    }

  }

  "A new 9x7 board" should {

    val board = new Board(9, 7)
    val cornerCoords  = Set(Coords(1, 1), Coords(1, board.height), Coords(board.width, 1), Coords(board.width, board.height))
    val illegalCoords = Set(Coords(0, 0), Coords(-1, -1), Coords(board.height + 1, board.height + 1))

    "be empty when created" in {
      board.toString must_== "+++++++++\n" * 7
    }

    "be 9x7" in {
      (board.width must_== 9) and (board.height must_== 7)
    }

    "have black and white captured count of 0" in {
      (board.capturesForColors(Black) must_== 0) and (board.capturesForColors(White) must_== 0)
    }

    "allow playing a black stone at any intersection" in {
      board.allCoords.foreach {
        board.canPlay(Black, _) must beTrue
      }
    }

    illegalCoords.foreach {
      pos => {
        "disallow playing a black stone at " + pos in {
          board.canPlay(Black, pos) must throwA [IllegalArgumentException]
        }

        "disallow getting a stone at " + pos in {
          board.get(pos) must throwA [IllegalArgumentException]
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
      board.allCoords.foreach(board.neighbors(_).foreach(_.isEmpty must beTrue))
    }

    "have a neighbor count of 4 for all intersections not near the edges" in {
      (2 until board.width) foreach {
        x => (2 until board.height) foreach {
          y => board.neighbors(Coords(x, y)).size must_== 4
        }
      }
    }

    "have a neighbor count of 2 for intersections in the four corners" in {
      cornerCoords.foreach(board.neighbors(_).size must_== 2)
    }

    "have a neighbor count of 3 for intersections near the edges but not in corners" in {

      val mustHaveThreeNeighbors = (pos: Coords) => if (!cornerCoords.contains(pos)) board.neighbors(pos).size must_== 3

      // First row and last row
      Seq(1, board.height).foreach { y =>
        (2 until board.width).foreach(x => mustHaveThreeNeighbors(Coords(x, y)))
      }
      // First col and last col
      Seq(1, board.width).foreach{ x =>
        (2 until board.height).foreach(y => mustHaveThreeNeighbors(Coords(x, y)))
      }
    }

  }

  "Playing the first stone on an empty board" should {

    val emptyBoard = new Board(9, 7)
    val board      = emptyBoard.play(1, 2)
    val neighbors  = Seq(Coords(1, 1), Coords(2, 2), Coords(1,3))

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
      pos => {
        "make neighbor at " + pos + " have exactly one neighbor" in {
          board.neighbors(pos).count(_ isDefined) must_== 1
        }

        "make neighbor at " + pos + " have a black neighbor" in {
          board.neighbors(pos).find(_ isDefined).get.get must_== Black
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

    "make black player's captured count 1" in {
      board.capturesForColors(Black) must_== 1
    }

  }

  "A board with a lonely black diamond where white tries to play inside the diamond" should {

    val board = new Board(9, 7).play(5, 3).endTurn.play(5, 5).endTurn.play(4, 4).endTurn.play(6, 4).play(5, 4)

    "keep white still in turn" in {
      board.whoseTurn must_== White
    }

    "disallow playing a white stone inside the diamond eye" in {
      board.canPlay(White, 5, 4) must beFalse
    }

    "allow playing a black stone inside the diamond eye" in {
      board.endTurn.canPlay(Black, 5, 4) must beTrue
    }

    "not add white stones to the board string" in {
      board.toString must not contain White
    }

    "keep the diamond eye empty" in {
      board.get(5, 4).isEmpty must beTrue
    }

  }

  "A board with a surrounded white diamond" should {

    val board = new Board(5, 5).play(3, 2).play(3, 1).play(2, 3).play(2, 2).play(3, 4).play(1, 3).play(4, 3).play(2, 4).endTurn.play(3, 5).endTurn.play(4, 4).endTurn.play(5, 3).endTurn.play(4, 2)

    "disallow playing a white stone that takes its own group's last liberty" in {
      board.canPlay(Black, 3, 3) must beFalse
      board.play(3, 3).whoseTurn must_== Black
      board.play(3, 3).capturesForColors(Black) must_== 0
      board.play(3, 3).capturesForColors(White) must_== 0
    }

  }

  "Taking the last liberty from a white two-stone group" should {

    val board = new Board(9, 7).play(3, 4).play(4, 4).play(4, 3).play(5, 4).play(5,3).endTurn.play(6, 4).endTurn.play(4, 5).endTurn.play(5,5)

    "make it disappear from the board string" in {

      board.toString must_==
      "+++++++++\n" +
      "+++++++++\n" +
      "+++bb++++\n" +
      "++b++b+++\n" +
      "+++bb++++\n" +
      "+++++++++\n" +
      "+++++++++\n"
    }

    "make it unretrievable from the position" in {
      board.get(4, 4).isEmpty must beTrue
      board.get(5, 4).isEmpty must beTrue
    }

    "make black player's captured count 2" in {
      board.capturesForColors(Black) must_== 2
    }

  }

  "Filling a 3x3 board with a black one-eyed group" should {

    val board = new Board(3, 3).play(1, 1).endTurn.play(2, 1).endTurn.play(3, 1).endTurn
                               .play(1, 2).endTurn.play(2, 2).endTurn.play(3, 2).endTurn
                               .play(1, 3).endTurn.play(2, 3)

    "allow playing a white stone inside the eye" in {
      board.canPlay(White, 3, 3) must beTrue
    }

    "update the board string accordingly after capture" in {
      board.play(3, 3).toString must_==
      "+++\n" +
      "+++\n" +
      "++w\n"
    }

    "make white player's captured count equal board size - 1 after capture" in {
      board.play(3, 3).capturesForColors(White) must_== board.width * board.height - 1
    }

  }

  "Taking the last liberty from a single stone whose group still has liberties" should {

    val board = new Board(3, 3).play(1, 1).play(2, 2).play(1, 2).play(1, 3)

    "keep the group in the board string" in {
      board.toString must_==
      "b++\n" +
      "bw+\n" +
      "w++\n"
    }

  }

  "Taking the last liberty from a single stone on a board with multiple groups of the same color" should {

    val board = new Board(3, 3).play(1, 1).endTurn.play(3, 1).endTurn.play(1, 3).endTurn.play(3, 3).play(3, 2).endTurn.play(2, 3)

    "update the board string accordingly" in {
      board.toString must_==
      "b+b\n" +
      "++w\n" +
      "bw+\n"
    }

  }

  "White player capturing a ko" should {

    val board = new Board(4, 3).play(2, 1).play(3, 1).play(1, 2).play(4, 2).play(2, 3).play(3, 3).play(3, 2).play(2, 2)

    "disallow playing a black stone that recaptures the ko" in {
      board.play(3, 2).capturesForColors(Black) must_== 0
      board.play(3, 2).whoseTurn must_== Black
    }

  }

  "An empty corner intersection with zero liberties and with neighboring black and white stones with liberties" should {

    val board = new Board(2, 2).play(1, 1).play(2, 2)

    "allow playing black or white in the corner" in {
      board.play(1, 2).get(1, 2).get must_== Black
      board.endTurn.play(1, 2).get(1, 2).get must_== White
    }

  }

}
