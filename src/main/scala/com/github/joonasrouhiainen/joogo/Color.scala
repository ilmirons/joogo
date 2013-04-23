package com.github.joonasrouhiainen.joogo

/**
 * Represents a go stone color.
 *
 * @author Joonas Rouhiainen
 */
sealed trait Color {

  implicit def color2string(c: Color) = c toString

  def invert: Color = this match {
    case Black => White
    case White => Black
  }

  override def toString: String = this match {
    case Black => "b"
    case White => "w"
  }

}

case object Black extends Color
case object White extends Color