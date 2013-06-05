package com.github.joonasrouhiainen.joogo

class MainServlet extends JoogoStack {

  val storage = new RuntimeGameStorage
  var id = ""

  newGame(9, 9)

  def store(g: Game): Unit = {
    id = storage.storeGame(g)
  }

  def game: Game = storage.getGame(id).get

  def newGame(x: Int, y: Int) = {
    store(new Game(x, y).addPlayer(Black, new Player("black")).addPlayer(White, new Player("white")))
  }

  def serveGame = {
    contentType = "text/html"
    jade("index", "game" -> game)
  }

  get("/") {
    serveGame
  }

  post("/") {
    if (params.get("x").isDefined && params.get("y").isDefined) {
      val x = params.get("x").get.toInt
      val y = params.get("y").get.toInt
      store(game.play(x, y))
    }
    serveGame
  }

  post("/pass") {
    store(game.pass)
    serveGame
  }

  post("/resign") {
    store(game.resign)
    serveGame
  }

  post("/new") {
    if (params.get("x").isDefined && params.get("y").isDefined) {
      val x = params.get("x").get.toInt
      val y = params.get("y").get.toInt
      newGame(x, y)
    }
    serveGame
  }
  
}
