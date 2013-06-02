package com.github.joonasrouhiainen.joogo

import org.specs2.mutable._

class GameSpec extends Specification {

  "A new game on a 19x19 board" should {

    val game = new Game(19, 19)

    "have a move number of 1" in {
      game.moveNumber must_== 1
    }

    "have no players" in {
      (game.players(Black) must_== None) and (game.players(White) must_== None)
    }

    "be unfinished" in {
      game.isFinished must beFalse
    }

    "have no result" in {
      game.result must beNone
    }

    "have a 19x19 board" in {
      (game.board.width must_== 19) and (game.board.height must_== 19)
    }

    "disallow playing moves or passing without players" in {
      Seq(Black, White).foreach { color =>
        game.play(Coords(1, 1)) must throwA[NoPlayerException]
        game.pass must throwA[NoPlayerException]
      }
    }

  }

  "A new game with two players" should {

    val black = new Player("b")
    val white = new Player("w")
    val gameWithPlayers = new Game(3).addPlayer(Black, black).addPlayer(White, white)

    "make the players added to it retrievable" in {
      (gameWithPlayers.players(Black) must_== Some(black)) and (gameWithPlayers.players(White) must_== Some(white))
    }

    "allow black to make a valid pass" in {
      val onePass = gameWithPlayers.pass
      onePass.isFinished must beFalse
      onePass.moveNumber must_== 2
      onePass.whoseTurn must_== White
    }

    "allow making a black move that updates the move number and adds the stone" in {
      val oneMove = gameWithPlayers.play(2, 2)
      oneMove.moveNumber must_== 2
      oneMove.board.get(2, 2).get must_== Black
      oneMove.boardStates.size must_== 2
    }

    "allow players to resign" in {
      gameWithPlayers.resign.result.get      must_== Black.resigned
      gameWithPlayers.pass.resign.result.get must_== White.resigned
    }

    "allow making a black pass and a white pass to end the game on white's turn" in {
      val twoPasses = gameWithPlayers.pass.pass
      twoPasses.isFinished must beTrue
      twoPasses.board.whoseTurn must_== White
    }

    "end the game with the correct result after one move and two passes" in {
      val oneBlackAndPassPass = gameWithPlayers.play(2, 2).pass.pass
      val allPoints = oneBlackAndPassPass.board.width * oneBlackAndPassPass.board.height
      oneBlackAndPassPass.isFinished must beTrue
      oneBlackAndPassPass.result.get must_== Black.wonByScore(Map(Black -> allPoints, White -> 0))
    }

    "end the game with the correct result when both players have encircled one point of territory and passed" in {
      val cornerForBoth = gameWithPlayers.play(1, 2).play(3, 2).play(2, 1).play(2, 3).pass.pass
      cornerForBoth.isFinished must beTrue
      cornerForBoth.result.get must_== new Draw(Map(Black -> 3, White -> 3))
    }

  }

}
