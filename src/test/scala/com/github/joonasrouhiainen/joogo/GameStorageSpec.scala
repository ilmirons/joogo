package com.github.joonasrouhiainen.joogo

import org.specs2.mutable.Specification
import com.github.joonasrouhiainen.joogo.model.{Player, Game, Coords}
import com.github.joonasrouhiainen.joogo.data.{RuntimeGameStorage, GameStorage}

/**
 * Unit tests for GameStorage.
 *
 * @author Joonas Rouhiainen
 */
class GameStorageSpec extends Specification {

  val game = new Game(9).addPlayer(Black, new Player("b")).addPlayer(White, new Player("w")).play(Coords(1, 1)).play(Coords(1, 1)).play(Coords(1, 2)).pass

  "A game data storage" should {

    val storage: GameStorage = new RuntimeGameStorage()

    "Allow saving and retrieving a game so that the retrieved game is equal to the saved one" in {
      val id = storage.storeGame(game)
      storage.getGame(id).isDefined must beTrue
      storage.getGame(id).get must_== game
      storage.gamesCount must_== 1
    }

  }

  "Trying to save the same game multiple times" should {

    val storage: GameStorage = new RuntimeGameStorage()

    "not be allowed" in {
      for (i <- 1 to 10) storage.storeGame(game)
      storage.gamesCount must_== 1
    }

  }

}
