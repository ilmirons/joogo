package com.github.joonasrouhiainen.joogo

import org.specs2.mutable._
import com.github.joonasrouhiainen.joogo.model._

class GameSpec extends Specification {

  "A new game on a 19x19 board" should {

    val game = new Game(19, 19, Map(Black -> new Player("b"), White -> new Player("b")))

    "have a move number of 1" in {
      game.moveNumber must_== 1
    }

    "have two players" in {
      game.players.size must_== 2
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

  }

  "A new game with one player" should {

    "be disallowed" in {
      new Game(3, 3, Map(Black -> new Player("lonely"))) must throwA [IllegalArgumentException]
    }

  }

  "A new game with two players" should {

    val black = new Player("b")
    val white = new Player("w")
    val gameWithPlayers = new Game(3, 3, Map(Black -> black, White -> white))

    "disallow fetching move numbers that are zero, negative or over current move number" in {
      gameWithPlayers.boardOnMove(0) must throwA[IllegalArgumentException]
      gameWithPlayers.boardOnMove(-3) must throwA[IllegalArgumentException]
      gameWithPlayers.boardOnMove(gameWithPlayers.moveNumber + 1) must throwA[IllegalArgumentException]
    }

    "make the players added to it retrievable" in {
      (gameWithPlayers.players(Black) must_== black) and (gameWithPlayers.players(White) must_== white)
    }

    "allow black to make a valid pass" in {
      val onePass = gameWithPlayers.pass
      onePass.isFinished must beFalse
      onePass.moveNumber must_== 2
      onePass.whoseTurn must_== White
    }

    "allow making a black move that updates the move number and adds the stone" in {
      val oneMove = gameWithPlayers.move(2, 2)
      oneMove.moveNumber must_== 2
      oneMove.board.get(2, 2).get must_== Black
    }

    "allow players to resign" in {
      val blackResigned = gameWithPlayers.resign.result.get
      val whiteResigned = gameWithPlayers.pass.resign.result.get
      blackResigned must_== Black.resigned
      blackResigned.toString must_== White + "+res."
      whiteResigned must_== White.resigned
      whiteResigned.toString must_== Black + "+res."
    }

    "allow making a black pass and a white pass to end the game on white's turn" in {
      val twoPasses = gameWithPlayers.pass.pass
      twoPasses.isFinished must beTrue
      twoPasses.board.whoseTurn must_== White
    }

    "not allow resigning after the game already has another result" in {
      val twoPasses = gameWithPlayers.pass.pass
      twoPasses.resign must throwA [GameAlreadyFinishedException]
    }

    "end the game with the correct result after one move and two passes" in {
      val oneBlackAndPassPass = gameWithPlayers.move(2, 2).pass.pass
      val allPoints = oneBlackAndPassPass.board.width * oneBlackAndPassPass.board.height
      val result = oneBlackAndPassPass.result.get

      oneBlackAndPassPass.isFinished must beTrue
      result must_== Black.wonByScore(Map(Black -> allPoints, White -> 0))
      result.toString must_== Black + "+" + allPoints.toFloat
    }

    "end the game with the correct result when both players have encircled one point of territory and passed" in {
      val cornerForBoth = gameWithPlayers.move(1, 2).move(3, 2).move(2, 1).move(2, 3).pass.pass
      val result = cornerForBoth.result.get

      cornerForBoth.isFinished must beTrue
      result must_== new Draw(Map(Black -> 3, White -> 3))
      result.toString must_== "Draw"
    }

    "should not react to illegal moves" in {
      val oneMove = gameWithPlayers.move(1, 1)
      val twoMovesAtSameIntersection = oneMove.move(1, 1)
      twoMovesAtSameIntersection must_== oneMove
    }

  }

}
