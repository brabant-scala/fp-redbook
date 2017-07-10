package nl.hugo.redbook.ch10

import nl.hugo.redbook.ch10.Monoid._
import nl.hugo.redbook.ch6.RNG
import nl.hugo.redbook.ch8.Prop.{ Falsified, Passed, Proved }
import nl.hugo.redbook.ch8._
import org.scalatest.{ Matchers, WordSpec }

class Test10_04 extends WordSpec with Matchers {
  "The monoidLaws" should {
    val smallIntGen = Gen.choose(-1000, 1000)
    "hold for the intAddition monoid" in {
      PropRunner.run(monoidLaws(intAddition, smallIntGen)) shouldBe true
    }

    "hold for the intMultiplication monoid" in {
      PropRunner.run(monoidLaws(intMultiplication, smallIntGen)) shouldBe true
    }

    "hold for the booleanOr monoid" in {
      PropRunner.run(monoidLaws(booleanOr, Gen.boolean)) shouldBe true
    }

    "hold for the booleanAnd monoid" in {
      PropRunner.run(monoidLaws(booleanAnd, Gen.boolean)) shouldBe true
    }

    "hold for the optionMonoid" in {
      val optionGen: Gen[Option[Int]] = Gen.union(smallIntGen.map(Some(_)), Gen.unit(None))

      PropRunner.run(monoidLaws(optionMonoid[Int], optionGen)) shouldBe true
    }
  }
}