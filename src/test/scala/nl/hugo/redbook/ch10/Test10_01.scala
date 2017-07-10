package nl.hugo.redbook.ch10

import org.scalatest.{ Matchers, WordSpec }

import nl.hugo.redbook.ch10.Monoid._

class Test10_01 extends WordSpec with Matchers {
  "An intAddition monoid" should {
    "return 'zero' when both arguments are 'zero'" in {
      intAddition.op(intAddition.zero, intAddition.zero) shouldBe intAddition.zero
    }

    "add two number" in {
      intAddition.op(100, 2000) shouldBe 100 + 2000
    }

    "return right argument when left argument is 'zero'" in {
      intAddition.op(intAddition.zero, 2000) shouldBe 2000
    }

    "return left argument when right argument is 'zero'" in {
      intAddition.op(100, intAddition.zero) shouldBe 100
    }
  }

  "An intMultiplication monoid" should {
    "return 'zero' when both arguments are 'zero'" in {
      intMultiplication.op(intMultiplication.zero, intMultiplication.zero) shouldBe intMultiplication.zero
    }

    "add two number" in {
      intMultiplication.op(100, 2000) shouldBe 100 * 2000
    }

    "return right argument when left argument is 'zero'" in {
      intMultiplication.op(intMultiplication.zero, 2000) shouldBe 2000
    }

    "return left argument when right argument is 'zero'" in {
      intMultiplication.op(100, intMultiplication.zero) shouldBe 100
    }
  }

  "An booleanOr  monoid" should {
    "return 'zero' when both arguments are 'zero'" in {
      booleanOr.op(booleanOr.zero, booleanOr.zero) shouldBe booleanOr.zero
    }

    "or two booleans" in {
      booleanOr.op(false, false) shouldBe false
      booleanOr.op(true, false) shouldBe true
      booleanOr.op(false, true) shouldBe true
      booleanOr.op(true, true) shouldBe true
    }

    "return right argument when left argument is 'zero'" in {
      booleanOr.op(booleanOr.zero, true) shouldBe true
      booleanOr.op(booleanOr.zero, false) shouldBe false
    }

    "return left argument when right argument is 'zero'" in {
      booleanOr.op(true, booleanOr.zero) shouldBe true
      booleanOr.op(false, booleanOr.zero) shouldBe false
    }
  }

  "An booleanAnd  monoid" should {
    "return 'zero' when both arguments are 'zero'" in {
      booleanAnd.op(booleanAnd.zero, booleanAnd.zero) shouldBe booleanAnd.zero
    }

    "or two booleans" in {
      booleanAnd.op(false, false) shouldBe false
      booleanAnd.op(true, false) shouldBe false
      booleanAnd.op(false, true) shouldBe false
      booleanAnd.op(true, true) shouldBe true
    }

    "return right argument when left argument is 'zero'" in {
      booleanAnd.op(booleanAnd.zero, true) shouldBe true
      booleanAnd.op(booleanAnd.zero, false) shouldBe false
    }

    "return left argument when right argument is 'zero'" in {
      booleanAnd.op(true, booleanAnd.zero) shouldBe true
      booleanAnd.op(false, booleanAnd.zero) shouldBe false
    }
  }
}