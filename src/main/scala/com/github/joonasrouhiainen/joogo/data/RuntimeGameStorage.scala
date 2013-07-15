package com.github.joonasrouhiainen.joogo.data

import com.github.joonasrouhiainen.joogo.model.Game

class RuntimeGameStorage extends GameStorage {

  var storage = Map[String, Game]()

  // Identify by players, enough for test purposes but two players can't have multiple games together
  private def id(game: Game): String = game.players.hashCode.toString

  override def games: Seq[Game] = storage.values.toSeq

  override def gamesCount: Int = storage.size

  override def getGame(id: String): Option[Game] = storage.get(id)

  override def storeGame(game: Game): String = {
    if (storage.exists(pair => pair._2 == game)) {
      storage.find(pair => pair._2 == game).get._1
    }
    else {
      storage += (id(game) -> game)
      id(game)
    }
  }

}
