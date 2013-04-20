package com.github.joonasrouhiainen.joogo

sealed trait Color {
  override def toString: String = this match {
    case Black => "b"
    case White => "w"
  }
}

case object Black extends Color
case object White extends Color