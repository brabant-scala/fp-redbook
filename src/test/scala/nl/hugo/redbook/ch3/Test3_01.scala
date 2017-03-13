package nl.hugo.redbook.ch3

import org.scalatest._

class Test3_01 extends WordSpec with Matchers {
  import Exercise01.x

  "Match" should {

    "pick the i-th option" in {
      x should be(3)
    }
  }
}