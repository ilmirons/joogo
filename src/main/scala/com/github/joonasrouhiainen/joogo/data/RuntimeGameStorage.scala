package com.github.joonasrouhiainen.joogo.data

import scala.util.Random
import com.github.joonasrouhiainen.joogo.model.Game

class RuntimeGameStorage extends GameStorage {

  var storage = Map[String, Game]()

  override def gamesCount(): Int = storage.size

  override def getGame(id: String): Option[Game] = storage.get(id)

  override def storeGame(game: Game): String = {
    if (storage.exists(pair => pair._2 == game)) {
      storage.find(pair => pair._2 == game).get._1
    }
    else {
      val id: String = Random.alphanumeric.slice(0, 10).mkString
      storage += (id -> game)
      id
    }
  }

}
