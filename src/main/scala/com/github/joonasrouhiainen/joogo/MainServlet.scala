package com.github.joonasrouhiainen.joogo

import com.github.joonasrouhiainen.joogo.model._
import com.github.joonasrouhiainen.joogo.data.RuntimeGameStorage

class MainServlet extends JoogoStack {

  val storage = new RuntimeGameStorage

  newGame(9, 9)

  def store(g: Game): String = storage.storeGame(g)
  def game(id: String): Option[Game] = storage.getGame(id)

  def newGame(x: Int, y: Int): String = {
    store(new Game(x, y).addPlayer(Black, new Player("black")).addPlayer(White, new Player("white")))
  }

  def serveCreation() = {
    contentType = "text/html"
    jade("index")
  }

  def serveGame(id: String) = {
    contentType = "text/html"
    jade("game", "game" -> storage.getGame(id).get, "gameId" -> id)
  }

  get("/")  {
    serveCreation
  }

  get("/g/:id") {
    serveGame(params("id"))
  }

  post("/g/:id") {
    if (params.get("x").isDefined && params.get("y").isDefined) {
      val x = params.get("x").get.toInt
      val y = params.get("y").get.toInt
      val newId = store(game(params("id")).get.play(x, y))
      serveGame(newId)
    }
    else serveGame(params("id"))
  }

  post("/g/:id/pass") {
    store(game(params("id")).get.pass)
    serveGame(params("id"))
  }

  post("/g/:id/resign") {
    store(game(params("id")).get.resign)
    serveGame(params("id"))
  }

  post("/") {
    if (params.get("x").isDefined && params.get("y").isDefined) {
      val x  = params("x").toInt
      val y  = params("y").toInt
      val id = newGame(x, y)
      serveGame(id)
    }
  }
  
}
