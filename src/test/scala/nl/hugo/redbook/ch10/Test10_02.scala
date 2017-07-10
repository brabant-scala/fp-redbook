package nl.hugo.redbook.ch10

import org.scalatest.{ Matchers, WordSpec }

import nl.hugo.redbook.ch10.Monoid._

class Test10_02 extends WordSpec with Matchers {
  "An optionMonoid" should {
    "return 'zero' when both arguments are 'zero'" in {
      optionMonoid.op(optionMonoid.zero, optionMonoid.zero) shouldBe optionMonoid.zero
    }

    "process two Some" in {
      // Exercise 10.02: You are free to choose the implementation
      optionMonoid.op(Some(1), Some(2)) shouldBe ???
    }

    "return right argument when left argument is 'zero'" in {
      optionMonoid.op(optionMonoid.zero, Some(2)) shouldBe Some(2)
    }

    "return left argument when right argument is 'zero'" in {
      optionMonoid.op(Some(1), optionMonoid.zero) shouldBe Some(1)
    }
  }

}