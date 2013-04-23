package com.github.joonasrouhiainen.joogo

import org.scalatra._
import scalate.ScalateSupport
import scala.xml.XML

class MainServlet extends JoogoStack with ScalateSupport {

  get("/") {
    contentType = "text/html"
    jade("index", "board" -> new Board(9))
  }

  post("/") {
    contentType = "text/html"

    if (params.get("x").isDefined && params.get("y").isDefined) {
      val x = params.get("x").get.toInt
      val y = params.get("y").get.toInt
      jade("index", "board" -> new Board(9).play(x, y))
    }
    jade("index", "board" -> new Board(9))
  }
  
}
