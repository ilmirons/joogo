package com.github.joonasrouhiainen.joogo

case class Game(players: Map[Color, Option[Player]], boardStates: Seq[Board], isFinished: Boolean) {

  def this(boardWidth: Int, boardHeight: Int) {
    this(Map(Black -> None, White -> None), Seq(new Board(boardWidth, boardHeight)), false)
  }

  def this(boardSize: Int) {
    this(boardSize, boardSize)
  }

  def addPlayer(color: Color, player: Player): Game = copy(players = players + (color -> Option(player)))

  def moveNumber: Int = boardStates.size

  def play(coords: Coords): Game = copy(boardStates = (boardStates.+:(boardStates.last.play(coords))))

}
