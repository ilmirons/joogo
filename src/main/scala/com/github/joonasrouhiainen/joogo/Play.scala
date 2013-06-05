package com.github.joonasrouhiainen.joogo

abstract sealed class Play(col: Color) {
  def color = col
}

case class Move(c: Color, pos: Coords) extends Play(c)

case class Pass(c: Color) extends Play(c)
