package nl.hugo.redbook.ch10

import nl.hugo.redbook.ch10.Monoid._
import nl.hugo.redbook.ch8.Gen
import org.scalatest.{ Matchers, WordSpec }

class Test10_18 extends WordSpec with Matchers {
  "Monoid.bag" should {
    "process the example" in {
      bag(Vector("a", "rose", "is", "a", "rose")) shouldBe Map("a" -> 2, "rose" -> 2, "is" -> 1)
    }

    "process an empty vector" in {
      bag(Vector.empty) shouldBe Map.empty
    }
  }
}