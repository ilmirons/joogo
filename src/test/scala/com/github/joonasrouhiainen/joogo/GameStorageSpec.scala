package com.github.joonasrouhiainen.joogo

import org.specs2.mutable.Specification
import com.github.joonasrouhiainen.joogo.model._
import com.github.joonasrouhiainen.joogo.data.{RuntimeGameStorage, GameStorage}

/**
 * Unit tests for GameStorage.
 *
 * @author Joonas Rouhiainen
 */
class GameStorageSpec extends Specification {

  val game = new Game(9, 9, Map(Black -> new Player("b"), White -> new Player("w"))).play(Black to (1, 1)).play(White to (1, 1)).play(Black to (1, 2)).pass

  "A game data storage" should {

    val storage: GameStorage = new RuntimeGameStorage

    "Allow saving and retrieving a game so that the retrieved game is equal to the saved one" in {
      val id = storage.storeGame(game)
      storage.getGame(id).isDefined must beTrue
      storage.getGame(id).get must_== game
      storage.gamesCount must_== 1
    }

  }

  "Trying to store the same game multiple times" should {

    val storage: GameStorage = new RuntimeGameStorage

    "not be allowed" in {
      for (i <- 1 to 10) storage.storeGame(game)
      storage.gamesCount must_== 1
    }

  }

  "Playing a move to a game and storing the game in between" should {

    val storage: GameStorage = new RuntimeGameStorage

    "not change the game's id in storage" in {
      val firstId  = storage.storeGame(game)
      val secondId = storage.storeGame(game.play(Black to (3, 3)))
      firstId must_== secondId
    }

  }

  "Storing three new games to a game storage" should {

    val storage: GameStorage = new RuntimeGameStorage

    "make the storage contain all three" in {
      val game2 = new Game(9, 9, Map(Black -> new Player("bl"), White -> new Player("wh")))
      val game3 = new Game(19, 19, Map(Black -> new Player("bla"), White -> new Player("whi")))

      storage.storeGame(game)
      storage.storeGame(game2)
      storage.storeGame(game3)

      storage.games.size must_== 3
      Seq(game, game2, game3).forall(storage.games.contains _) must beTrue
    }

  }

}
