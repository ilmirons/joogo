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

    "allow placing stones" ! !(new Board().isFull)

    "allow placing a stone at (1,1)" ! new Board().canPlace(1, 1)

    "disallow placing a stone at (0,0)" ! !new Board().canPlace(0, 0)
  }

}
