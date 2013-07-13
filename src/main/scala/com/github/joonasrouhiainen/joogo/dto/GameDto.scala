package com.github.joonasrouhiainen.joogo.dto

import com.github.joonasrouhiainen.joogo.model.{Player, Color, Game}

case class GameDto(
  board:             Iterable[Iterable[String]],
  capturesForColors: Map[String, String],
  playersForColors:  Map[String, String],
  whoseTurn:         String
)

object GameDto {

  implicit def game2GameDto(game: Game) =
    GameDto(
      game.board.toString.split("\n").map(_.toCharArray.map(_.toString).toSeq).toSeq,
      game.board.capturesForColors.map { case (c: Color, count: Int) => (c.toString, count.toString) },
      game.players.map { case (c: Color, p: Player) => (c.toString, p.id) },
      game.players(game.whoseTurn).id
    )
}