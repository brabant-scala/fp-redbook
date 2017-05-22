package nl.hugo.redbook.ch8

import nl.hugo.redbook.ch6.RNG
import org.scalatest.{Matchers, WordSpec}

class Test8_12 extends WordSpec with Matchers {
  "Gen.listOf" should {
    "return a sized generator that generates list of the requested size" in {
      val constGen = Gen.unit(1)

      val rng = RNG.Simple(System.nanoTime())

      val listOfProp = Prop.forAll(Gen.choose(0, 1000)) { n => Gen.listOf(Gen.unit(1))(n).sample.run(rng)._1 == List.fill(n)(1) }

      listOfProp.run(0, 100, rng) should be(Prop.Passed)
    }
  }
}
