package com.github.joonasrouhiainen.joogo

import org.scalatra._
import scalate.ScalateSupport

class MainServlet extends JoogoStack with ScalateSupport {

  var game = newGame(9, 9)
  newGame(9, 9)

  def newGame(x: Int, y: Int): Game = {
    new Game(x, y).addPlayer(Black, new Player("black")).addPlayer(White, new Player("white"))
  }

  def serveGame() = {
    contentType = "text/html"
    jade("index", "game" -> game)
  }

  get("/") {
    serveGame()
  }

  post("/") {
    if (params.get("x").isDefined && params.get("y").isDefined) {
      val x = params.get("x").get.toInt
      val y = params.get("y").get.toInt
      game = game.play(x, y)
    }
    serveGame()
  }

  post("/pass") {
    game = game.pass
    serveGame()
  }

  post("/resign") {
    game = game.resign
    serveGame()
  }

  post("/new") {
    if (params.get("x").isDefined && params.get("y").isDefined) {
      val x = params.get("x").get.toInt
      val y = params.get("y").get.toInt
      game = newGame(x, y)
    }
    serveGame()
  }
  
}
