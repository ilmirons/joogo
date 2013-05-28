package com.github.joonasrouhiainen.joogo

import org.specs2.mutable._
import akka.pattern.ask
import akka.testkit.TestActorRef
import scala.concurrent.{ExecutionContext, Future}
import ExecutionContext.Implicits.global
import scala.util.Try
import akka.actor.ActorSystem
import com.typesafe.config.ConfigFactory
import akka.util.Timeout

class GameSpec extends Specification {

  implicit val system = ActorSystem("TestSystem", ConfigFactory.load())
  implicit val timeout = Timeout(1000)

  "A new game on a 19x19 board" should {

    val game = new Game(19, 19)

    "have a move number of 0" in {
      game.moveNumber must_== 0
    }

    "have no players" in {
      (game.players(Black) must_== None) and (game.players(White) must_== None)
    }

    "be unfinished" in {
      game.isFinished must beFalse
    }

    "have a 19x19 board" in {
      (game.board.width must_== 19) and (game.board.height must_== 19)
    }
  }

  "Passing on a new game" should {

    val onePass = new Game(3).pass

    "not finish the game" in {
      onePass.isFinished must beFalse
    }

    "make the move number 1" in {
      onePass.moveNumber must_== 1
    }

  }

  "A game with two consecutive passes" should {

    val twoPasses = new Game(3).pass.pass

    "be finished" in {
      twoPasses.isFinished must beTrue
    }

    "have white still in turn" in {
      twoPasses.board.whoseTurn must_== White
    }

  }

  "A new game where black plays the first move" should {

    val oneMove = new Game(3).play(Black, Coords(2, 2))

    "have a move number of 1" in {
      oneMove.moveNumber must_== 1
    }

    "have a board with a black stone at the position" in {
      oneMove.board.get(2, 2).get must_== Black
    }

    "contain two board states" in {
      oneMove.boardStates.size must_== 2
    }

  }

  "A new game where white tries to play the first move" should {

    val oneWhiteMove = new Game(3).play(White, Coords(2, 2))

    "have a move number of 0" in {
      oneWhiteMove.moveNumber must_== 0
    }

    "have no white stones" in {
      oneWhiteMove.board.groups(White) must beEmpty
    }

  }

  "A new game with two players" should {

    val black = new Player("b")
    val white = new Player("w")
    val gameWithPlayers = new Game(3).addPlayer(Black, black).addPlayer(White, white)

    "make the players added to it retrievable" in {
      (gameWithPlayers.players(Black) must_== Some(black)) and (gameWithPlayers.players(White) must_== Some(white))
    }

  }

}
