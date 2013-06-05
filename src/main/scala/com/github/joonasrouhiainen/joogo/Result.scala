package com.github.joonasrouhiainen.joogo

// All possible outcomes of a finished game.

abstract sealed class Result(winner: Option[Color])

case class Draw(scoresForColors: Map[Color, Float]) extends Result(None) {
  override def toString = "Draw"
}

case class Resigned(win: Color) extends Result(Some(win)) {
  override def toString = win + "+res."
}

case class WonByScore(win: Color, scoresForColors: Map[Color, Float]) extends Result(Some(win)) {
  override def toString = win + "+" + (scoresForColors(win) - scoresForColors(win.invert))
}