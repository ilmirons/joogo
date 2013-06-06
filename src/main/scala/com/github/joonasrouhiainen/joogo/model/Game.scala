package com.github.joonasrouhiainen.joogo.model

/**
 * A game of go with chinese rules and two players.
 *
 * @author Joonas Rouhiainen
 */
case class Game private(players: Map[Color, Player], boardStates: Seq[Board], result: Option[Result]) {

  require(players.size == 2)

  def this(boardWidth: Int, boardHeight: Int, players: Map[Color, Player]) {
    this(players, Seq(new Board(boardWidth, boardHeight)), None)
  }

  /**
   * Returns the newest board state.
   */
  def board: Board = boardStates.head

  def isFinished: Boolean = result.isDefined

  def moveNumber: Int = boardStates.size

  def pass: Game = {
    require(!isFinished)
    val passingWillEndGame = moveNumber >= 2 && board.intersections == boardStates(1).intersections

    if (passingWillEndGame) copy(result = Some(chineseScore))
    else withMove(board.play(board.whoseTurn.pass))
  }

  def play(x: Int, y: Int): Game = play(Coords(x, y))

  def play(coords: Coords): Game = {
    require(!isFinished)
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