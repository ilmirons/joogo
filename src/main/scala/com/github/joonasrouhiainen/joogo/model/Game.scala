package com.github.joonasrouhiainen.joogo.model

/**
 * A game of go with chinese rules and two players.
 *
 * @author Joonas Rouhiainen
 */
case class Game private(boardWidth: Int, boardHeight: Int, players: Map[Color, Player], plays: List[Play], result: Option[Result]) {

  require(players.size == 2)

  def this(boardWidth: Int, boardHeight: Int, players: Map[Color, Player]) {
    this(boardWidth, boardHeight, players, List(), None)
  }

  /**
   * Returns the newest board state.
   */
  def board: Board = boardOnMove(moveNumber)

  def boardOnMove(number: Int): Board = {
    require(0 < number && number <= moveNumber)

    plays.takeRight(number).foldRight(new Board(boardWidth, boardHeight)) {
      (p, boardState) => boardState(p)
    }
  }

  def isFinished: Boolean = result.isDefined

  def moveNumber: Int = 1 + plays.size

  private def move(m: Move): Game = {
    if (board.canPlay(m)) {
      copy(plays = m :: plays)
    }
    else copy()
  }

  private def pass(p: Pass): Game = {
    val passingWillEndGame = moveNumber >= 2 && plays(0).isInstanceOf[Pass]

    if (passingWillEndGame) copy(result = Some(chineseScore))
    else                    copy(plays  = p :: plays)
  }

  // Convenience method for testing
  def pass: Game = play(whoseTurn pass)

  // Convenience method for testing
  def move(x: Int, y: Int): Game = {
    play(whoseTurn to (x, y))
  }

  def play(p: Play): Game = {
    require(!isFinished)

    p match {
      case m: Move => move(m)
      case p: Pass => pass(p)
    }
  }

  def resign: Game = {
    if (result isDefined) throw new GameAlreadyFinishedException
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

}

class GameAlreadyFinishedException extends Exception