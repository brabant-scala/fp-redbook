package nl.hugo.redbook.ch5

import org.scalatest.{ Matchers, WordSpec }

class Test5_01 extends WordSpec with Matchers {
  "A stream" should {
    "turn a non-empty stream into a list" in {
      Stream(1, 2, 3).toList should be(List(1, 2, 3))
    }

    "turn an empty stream into Nil" in {
      Empty.toList should be(Nil)
    }
  }
}