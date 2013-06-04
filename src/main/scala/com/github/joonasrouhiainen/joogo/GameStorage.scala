package com.github.joonasrouhiainen.joogo

trait GameStorage {

  def gamesCount: Int
  def getGame(id: String): Option[Game]
  def storeGame(game: Game): String

}
