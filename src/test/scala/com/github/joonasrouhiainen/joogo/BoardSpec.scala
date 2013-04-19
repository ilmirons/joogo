package com.github.joonasrouhiainen.joogo

import org.specs2.mutable._

class BoardSpec extends Specification {

  "A new board" should {
    "be empty when created" in {
      new Board().toString ==
        "+++++++++\n" +
        "+++++++++\n" +
        "+++++++++\n" +
        "+++++++++\n" +
        "+++++++++\n" +
        "+++++++++\n" +
        "+++++++++\n" +
        "+++++++++\n" +
        "+++++++++\n"
    }
  }

}
