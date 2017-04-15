package nl.hugo.redbook.ch5

import org.scalatest._

class Test5_10 extends WordSpec with Matchers {
  "Stream" should {
    "generate fibonacci numbers" in {
      Stream.fibs.take(7).toList should be(List(0, 1, 1, 2, 3, 5, 8))
      Stream.fibs2.take(7).toList should be(List(0, 1, 1, 2, 3, 5, 8))
    }
  }
}
