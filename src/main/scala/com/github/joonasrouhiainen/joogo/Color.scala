package com.github.joonasrouhiainen.joogo

/**
 * Represents a go stone color.
 *
 * @author Joonas Rouhiainen
 */
sealed trait Color {

  implicit def color2string(c: Color) = c.toString

  def invert: Color = this match {
    case Black => White
    case White => Black
  }

  def resigned: Result = new Resigned(this)

  override def toString: String = this match {
    case Black => "b"
    case White => "w"
  }

  def wonByScore(scoresForColors: Map[Color, Float]): Result = new WonByScore(this, scoresForColors)

}

case object Black extends Color
case object White extends Color