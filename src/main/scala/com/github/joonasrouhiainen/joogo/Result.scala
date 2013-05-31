package com.github.joonasrouhiainen.joogo

// All possible outcomes of a finished game.

abstract class Result(winner: Option[Color])

case class Draw(scoresForColors: Map[Color, Float])
  extends Result(None)

case class Resigned(win: Color)
  extends Result(Some(win))

case class WonByScore(win: Color, scoresForColors: Map[Color, Float])
  extends Result(Some(win))