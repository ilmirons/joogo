package com.github.joonasrouhiainen.joogo

case class Game private(players: Map[Color, Option[Player]], boardStates: Seq[Board], isFinished: Boolean) {

  def this(boardWidth: Int, boardHeight: Int) {
    this(Map(Black -> None, White -> None), Seq(new Board(boardWidth, boardHeight)), false)
  }

  def this(boardSize: Int) {
    this(boardSize, boardSize)
  }

  def addPlayer(color: Color, player: Player): Game = copy(players = players + (color -> Option(player)))

  /**
   * Returns the newest board state.
   */
  def board: Board = boardStates.head

  private def ensurePlayerPresent(color: Color): Unit = {
    if (players(color).isEmpty) throw new NoPlayerException
  }

  def moveNumber: Int = boardStates.indices.last

  def pass(color: Color): Game = {
    ensurePlayerPresent(color)
    val passingWillEndGame = moveNumber >= 1 && board == boardStates(moveNumber - 1)

    if (passingWillEndGame) copy(isFinished = true)
    else {
      copy(boardStates = boardStates.+:(board.endTurn), isFinished = passingWillEndGame)
    }
  }

  def play(color: Color, coords: Coords): Game = {
    ensurePlayerPresent(color)
    if (board.whoseTurn == color) copy(boardStates = boardStates.+:(board.play(coords)))
    else copy()
  }

  def whoseTurn = board.whoseTurn

}

class NoPlayerException extends Exception
