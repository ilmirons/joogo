package com.github.joonasrouhiainen.joogo

/**
 * A game of go with chinese rules.
 *
 * @author Joonas Rouhiainen
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

  def moveNumber: Int = boardStates.size

  def pass: Game = {
    require(!isFinished)
    ensurePlayerPresent(whoseTurn)
    val passingWillEndGame = moveNumber >= 2 && board.intersections == boardStates(1).intersections

    if (passingWillEndGame) copy(result = Some(chineseScore))
    else withMove(board.play(board.whoseTurn.pass))
  }

  def play(x: Int, y: Int): Game = play(Coords(x, y))

  def play(coords: Coords): Game = {
    require(!isFinished)
    ensurePlayerPresent(whoseTurn)
    if (board.canPlay(whoseTurn to coords)) {
      withMove(board.play(whoseTurn to coords))
    }
    else copy()
  }

  def resign: Game = {
    copy(result = Some(whoseTurn resigned))
  }

  def chineseScore: Result = {
    var pointsForColors: Map[Color, Float] = Map()
    players.keySet.foreach(col => pointsForColors += col -> (board.territories(col).flatten.size + board.numStones(col)))

    val winner: (Color, Float) = pointsForColors.maxBy(_._2)
    val loser:  (Color, Float) = pointsForColors.minBy(_._2)

    if (loser._2 == winner._2) new Draw(pointsForColors)
    else new WonByScore(winner._1, pointsForColors)
  }

  def whoseTurn: Color = board.whoseTurn

  private def withMove(boardState: Board): Game = copy(boardStates = boardStates.+:(boardState))

}

class NoPlayerException extends Exception
