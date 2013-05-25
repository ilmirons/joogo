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
    val gameActor = TestActorRef(new GameActor(game))

    "have a move number of 1" in {
      game.moveNumber must_== 1
    }

    "have no players" in {
      (game.players(Black) must_== None) and (game.players(White) must_== None)
    }

    "be unfinished" in {
      game.isFinished must beFalse
    }

    "have a 19x19 board" in {
      val board = game.boardStates.head
      (board.width must_== 19) and (board.height must_== 19)
    }

    "have a board with a black stone after receiving a move" in {
      val future: Future[Game] = ask(gameActor, new Coords(2, 2)).mapTo[Game]

      future.onComplete {
        case boardAfterMove: Try[Game] => boardAfterMove.get.boardStates.head.get(2, 2).get must_== Black
        case _ => failure
      }
    }

  }

  "A new game with two players" should {

    val randomBlackPlayer = new RandomPlayer(3, 3)
    val randomWhitePlayer = new RandomPlayer(3, 3)
    val game = new Game(3).addPlayer(Black, randomBlackPlayer).addPlayer(White, randomWhitePlayer)

    "make the players added to it retrievable" in {
      game.players(Black) must_== Some(randomBlackPlayer)
      game.players(White) must_== Some(randomWhitePlayer)
    }

  }

}
