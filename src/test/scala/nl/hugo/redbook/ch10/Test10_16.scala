package nl.hugo.redbook.ch10

import nl.hugo.redbook.ch10.Monoid._
import nl.hugo.redbook.ch8.Gen
import org.scalatest.{ Matchers, WordSpec }

class Test10_16 extends WordSpec with Matchers {
  val L: Monoid[String] = stringMonoid
  val R: Monoid[Int] = intAddition

  "An product monoid" should {
    "return 'zero' when both arguments are 'zero'" in {
      productMonoid(L, R).op(productMonoid(L, R).zero, productMonoid(L, R).zero) shouldBe productMonoid(L, R).zero
    }

    "return right argument when left argument is 'zero'" in {
      productMonoid(L, R).op(productMonoid(L, R).zero, ("B", 10)) shouldBe ("B", 10)
    }

    "return left argument when right argument is 'zero'" in {
      productMonoid(L, R).op(("A", 1), productMonoid(L, R).zero) shouldBe ("A", 1)
    }

    "process two values" in {
      productMonoid(L, R).op(("A", 1), ("B", 10)) shouldBe ("AB", 11)
    }

    "obey the monoid laws" in {
      val intPairGen: Gen[(Int, Int)] =
        for {
          l <- Gen.weighted((Gen.unit(intAddition.zero), 0.1), (Gen.choose(0, 100), 0.9))
          r <- Gen.weighted((Gen.unit(intMultiplication.zero), 0.1), (Gen.choose(0, 100), 0.9))
        } yield (l, r)

      PropRunner.run(monoidLaws(productMonoid(intAddition, intMultiplication), intPairGen)) shouldBe true
    }
  }
}