package com.github.joonasrouhiainen.joogo

/**
 * A game of go with chinese rules.
 */
case class Game private(players: Map[Color, Option[Player]], boardStates: Seq[Board], result: Option[Result]) {

  def this(boardWidth: Int, boardHeight: Int) {
    this(Map(Black -> None, White -> None), Seq(new Board(boardWidth, boardHeight)), None)
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

  def isFinished: Boolean = result.isDefined

  def moveNumber: Int = boardStates.indices.last

  def pass: Game = {
    ensurePlayerPresent(whoseTurn)
    val passingWillEndGame = moveNumber >= 1 && board == boardStates(moveNumber - 1)

    if (passingWillEndGame) copy(result = Some(chineseScore))
    else copy(boardStates = boardStates.+:(board.endTurn))
  }

  def play(x: Int, y: Int): Game = play(Coords(x, y))

  def play(coords: Coords): Game = {
    ensurePlayerPresent(whoseTurn)
    copy(boardStates = boardStates.+:(board.play(coords)))
  }

  def resign: Game = {
    copy(result = Some(whoseTurn.resigned))
  }

  def chineseScore: Result = {
    var pointsForColors: Map[Color, Float] = Map()
    players.keySet.foreach(col => pointsForColors += col -> (board.territories(col).flatten.size + board.numStones(col)))

    val winner: (Color, Float) = pointsForColors.maxBy(_._2)
    val loser:  (Color, Float) = pointsForColors.minBy(_._2)

    if (loser._2 == winner._2) new Draw(pointsForColors)
    else new WonByScore(winner._1, pointsForColors)
  }

  def whoseTurn = board.whoseTurn

}

class NoPlayerException extends Exception
