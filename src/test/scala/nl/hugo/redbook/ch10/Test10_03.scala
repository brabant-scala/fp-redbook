package nl.hugo.redbook.ch10

import nl.hugo.redbook.ch10.Monoid._
import org.scalatest.{ Matchers, WordSpec }

class Test10_03 extends WordSpec with Matchers {
  "An endoMonoid" should {
    val monoid = endoMonoid[Int]
    "return 'zero' when both arguments are 'zero'" in {
      monoid.op(monoid.zero, monoid.zero)(1138) shouldBe endoMonoid.zero(1138)
    }

    "process two functions" in {
      // Exercise 10.02: You are free to choose the implementation
      //monoid.op(_ + 200, _ + 300)(1) shouldBe ???

    }

    "return right argument when left argument is 'zero'" in {
      monoid.op(endoMonoid.zero, _ + 20)(1) shouldBe 21
    }

    "return left argument when right argument is 'zero'" in {
      monoid.op(_ + 100, endoMonoid.zero)(2) shouldBe 102
    }
  }
}