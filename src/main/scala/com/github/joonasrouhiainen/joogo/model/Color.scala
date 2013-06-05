package com.github.joonasrouhiainen.joogo.model

/**
 * Represents a go stone color.
 *
 * @author Joonas Rouhiainen
 */
sealed trait Color {

  implicit def color2string(c: Color) = c.toString

  def invert: Color

  def resigned: Result = new Resigned(invert)

  def pass: Pass = new Pass(this)

  def to(coords: Coords): Move = new Move(this, coords)
  def to(x: Int, y: Int): Move = to(Coords(x, y))

  def wonByScore(scoresForColors: Map[Color, Float]): Result = new WonByScore(this, scoresForColors)

}

case object Black extends Color {
  def invert = White
  override def toString = "b"
}

case object White extends Color {
  def invert = Black
  override def toString = "w"
}