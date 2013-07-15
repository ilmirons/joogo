package com.github.joonasrouhiainen.joogo

import com.github.joonasrouhiainen.joogo.model._
import com.github.joonasrouhiainen.joogo.data.RuntimeGameStorage
import org.scalatra.SessionSupport

import org.scalatra.atmosphere._
import org.scalatra.json.{JValueResult, JacksonJsonSupport}
import org.json4s._
import JsonDSL._

import scala.concurrent._
import ExecutionContext.Implicits.global
import com.github.joonasrouhiainen.joogo.dto.GameDto

class MainServlet extends JoogoStack with JValueResult with JacksonJsonSupport with SessionSupport with AtmosphereSupport {

  implicit protected val jsonFormats: Formats = DefaultFormats

  val storage = new RuntimeGameStorage

  def store(g: Game): String = storage.storeGame(g)
  def game(id: String): Option[Game] = storage.getGame(id)

  def newGame(x: Int, y: Int, players: Map[Color, Player]): String = {
    store(new Game(x, y, players))
  }

  def gamePage(id: String) = {
    contentType = "text/html"
    jade("game", "game" -> storage.getGame(id).get, "gameId" -> id)
  }

  get("/") {
    contentType = "text/html"
    jade("index")
  }

  get("/g") {
    jade("list-games", "games" -> storage.games)
  }

  get("/g/:id") {
    gamePage(params("id"))
  }

  get("/API/g/:id") {
    contentType = formats("json")
    val id = params("id")
    storage.getGame(id).get: GameDto
  }

  def play(gameId: String, coords: Coords) {
    println("played at " + coords)
    val newId = store(game(gameId).get.move(coords.x, coords.y))
  }

  post("/g/:id/pass") {
    val newId = store(game(params("id")).get.pass)
    gamePage(newId)
  }

  post("/g/:id/resign") {
    val newId = store(game(params("id")).get.resign)
    gamePage(newId)
  }

  post("/") {
    if (params.get("x").isDefined && params.get("y").isDefined) {
      val x  = params("x").toInt
      val y  = params("y").toInt
      val players: Map[Color, Player] = Map(Black -> new Player(params("b")), White -> new Player(params("w")))

      val id = newGame(x, y, players)
      redirect("/g/" + id)
    }
  }

  atmosphere("/the-chat") {
    new AtmosphereClient {
      def receive: AtmoReceive = {
        case Connected => println("Client %s connected" format uuid)
        case Disconnected(ClientDisconnected, _) => println("Client %s disconnected" format uuid)
        case Disconnected(ServerDisconnected, _) => println("Server disconnected the client %s" format uuid)
        case JsonMessage(json) =>
          val coords = Coords((json \ "x").extract[Int], (json \ "y").extract[Int])
          val gameId = (json \ "game").extract[String]
          play(gameId, coords)

          send(compact("board" -> storage.getGame(gameId).get.board.toString))
      }
    }
  }
  
}
