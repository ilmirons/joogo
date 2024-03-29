package com.github.joonasrouhiainen.joogo

import org.scalatra.test.specs2._

class MainServletSpec extends MutableScalatraSpec {

  addServlet(classOf[MainServlet], "/*")

  "GET / on MainServlet" should {
    "return status 200" in {
      get("/") {
        status must_== 200
      }
    }
  }

  "POST / on MainServlet" should {
    "return status 200" in {
      post("/") {
        status must_== 200
      }
    }
  }

}
