package nl.hugo.redbook.ch10

import nl.hugo.redbook.ch10.Monoid._
import nl.hugo.redbook.ch8.Gen
import org.scalatest.{ Matchers, WordSpec }

class Test10_17 extends WordSpec with Matchers {
  val SM: Monoid[String] = stringMonoid
  val IAM: Monoid[Int] = intAddition
  val IMM: Monoid[Int] = intMultiplication

  "An product monoid" should {
    "return 'zero' when both arguments are 'zero'" in {
      functionMonoid(SM).op(functionMonoid(SM).zero, functionMonoid(SM).zero)("FOOBAR") shouldBe SM.zero
      functionMonoid(IAM).op(functionMonoid(IAM).zero, functionMonoid(IAM).zero)(1) shouldBe IAM.zero
      functionMonoid(IMM).op(functionMonoid(IMM).zero, functionMonoid(IMM).zero)(100) shouldBe IMM.zero
    }

    "return right argument when left argument is 'zero'" in {
      functionMonoid[String, String](SM).op(functionMonoid(SM).zero, s => s"S$s")("test") shouldBe "Stest"

    }

    "return left argument when right argument is 'zero'" in {
      functionMonoid[String, String](SM).op(s => s"F$s", functionMonoid(SM).zero)("test") shouldBe "Ftest"
    }

    "process two values" in {
      functionMonoid[String, String](SM).op(s => s"F$s", s => s"S$s")("test") shouldBe "FtestStest"
    }
  }
}