package com.github.joonasrouhiainen.joogo

sealed trait Color {
  def toChar: Char = this match {
    case Black => 'b'
    case White => 'w'
  }
}

case object Black extends Color
case object White extends Color