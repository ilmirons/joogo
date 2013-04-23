package com.github.joonasrouhiainen.joogo

import org.scalatra._
import scalate.ScalateSupport
import scala.xml.XML

class MainServlet extends JoogoStack with ScalateSupport {

  var board = new Board(9)

  get("/") {
    contentType = "text/html"
    jade("index", "board" -> board)
  }

  post("/") {
    contentType = "text/html"

    if (params.get("x").isDefined && params.get("y").isDefined) {
      val x = params.get("x").get.toInt
      val y = params.get("y").get.toInt
      board = board.play(x, y)
    }
    jade("index", "board" -> board)
  }
  
}
