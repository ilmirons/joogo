package com.github.joonasrouhiainen.joogo

/**
 * Wrapper for two-dimensional board coordinates.
 *
 * @author Joonas Rouhiainen
 */
case class Coords(x: Int, y: Int)

object Coords {

  /**
   * Returns all legal coordinates for a board of given width and height.
   */
  def all(width: Int, height: Int): Seq[Coords] = {
    var allPositions = Seq[Coords]()

    (1 to width).foreach {
      x => (1 to height).foreach {
        y => {
          allPositions = allPositions :+ Coords(x, y)
        }
      }
    }
    allPositions
  }

}