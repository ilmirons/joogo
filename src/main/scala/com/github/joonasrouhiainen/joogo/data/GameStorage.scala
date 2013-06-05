package com.github.joonasrouhiainen.joogo.data

import com.github.joonasrouhiainen.joogo.model.Game

trait GameStorage {

  def gamesCount: Int
  def getGame(id: String): Option[Game]
  def storeGame(game: Game): String

}
