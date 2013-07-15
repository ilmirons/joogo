package com.github.joonasrouhiainen.joogo.data

import com.github.joonasrouhiainen.joogo.model.Game

/**
 * A stateful game storage.
 */
trait GameStorage {

  def games: Seq[Game]
  def gamesCount: Int
  def getGame(id: String): Option[Game]
  def storeGame(game: Game): String

}
