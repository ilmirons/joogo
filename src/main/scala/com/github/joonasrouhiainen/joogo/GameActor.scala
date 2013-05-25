package com.github.joonasrouhiainen.joogo

import akka.actor.{ActorLogging, Actor}

class GameActor(game: Game) extends Actor with ActorLogging {

  def receive = {
    case pos: Coords => sender ! game.play(pos)
    case _ => sender ! game
  }

}
