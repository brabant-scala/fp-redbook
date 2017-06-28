package nl.hugo.redbook.ch8

import nl.hugo.redbook.ch6.RNG
import org.scalatest.{ Matchers, WordSpec }

class Test8_13 extends WordSpec with Matchers {
  "Gen.listOf1" should {
    "return lists that are always nonEmpty" in {
      val constGen = Gen.unit(1)

      val listOf1Gen = Gen.listOf1(constGen)

      val rng = RNG.Simple(System.nanoTime())

      val listOfProp = Prop.forAll(Gen.choose(-100, 100)) { n => listOf1Gen(n).sample.run(rng)._1.nonEmpty }

      listOfProp.run(10, 1000, rng) should be(Prop.Passed)
    }
  }
}
