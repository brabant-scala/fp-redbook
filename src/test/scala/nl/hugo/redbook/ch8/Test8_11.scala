package nl.hugo.redbook.ch8

import nl.hugo.redbook.ch6.RNG
import org.scalatest.{Matchers, WordSpec}

class Test8_11 extends WordSpec with Matchers {
  val unitSGen = SGen { n => Gen.unit(n) }
  val rng = RNG.Simple(System.nanoTime())

  "SGen.apply" should {
    "return a generator based on that value" in {
      val applyProp = Prop.forAll(Gen.choose(0, Int.MaxValue)) { n => unitSGen(n).sample.run(rng)._1 == n }

      applyProp.run(0, 100, rng) should be(Prop.Passed)
    }
  }

  "SGen.map" should {
    "convert one action into another" in {
      val mapProp = Prop.forAll(Gen.choose(0, Int.MaxValue)) { n => unitSGen.map(_ - 1)(n).sample.run(rng)._1 == n - 1 }

      mapProp.run(0, 100, rng) should be(Prop.Passed)
    }
  }

  "SGen.flatMap" should {
    "concatenate the values in a string" in {
      def f = (i: Int) => SGen { n => Gen.unit(s"$i$n") }

      val flatMapProp = Prop.forAll(Gen.choose(-1000, 1000))(n => unitSGen.flatMap(f)(n).sample.run(rng)._1 == s"$n$n")

      flatMapProp.run(0, 100, rng) should be(Prop.Passed)
    }
  }

  "SGen.**" should {
    "combine two generators into one pair" in {
      val paired = unitSGen ** unitSGen.map(_ + 1)

      val pairProp = Prop.forAll(Gen.choose(-1000, 1000))(n => paired(n).sample.run(rng)._1 == (n, n + 1))

      pairProp.run(0, 100, rng) should be(Prop.Passed)
    }
  }

}
