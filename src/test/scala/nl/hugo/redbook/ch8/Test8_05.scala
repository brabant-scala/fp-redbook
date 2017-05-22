package nl.hugo.redbook.ch8

import nl.hugo.redbook.ch6.RNG
import org.scalatest.{ Matchers, WordSpec }

class Test8_05 extends WordSpec with Matchers {
  "A generator" should {
    "produce a unit" in {
      for (n <- 0 to 10000) {
        val rng: RNG = RNG.Simple(n)
        val (v, rng2) = RNG.int(rng)
        Gen.unit(v).sample.run(rng2)._1 should be(v)
      }

    }

    "produce a boolean" in {
      val rng: RNG = RNG.Simple(0)
      val (v, rng2) = Gen.boolean.sample.run(rng)
      v should be(RNG.boolean(rng)._1)
    }

    "produce a listOfN" in {
      for (n <- 0 to 100) {
        val rng: RNG = RNG.Simple(n)
        val (v, rng2) = RNG.nonNegativeLessThan(100)(rng)
        val (l, _) = Gen.listOfN(v, Gen.unit(0)).sample.run(rng)
        l.size should be(v)
      }
    }
  }
}
