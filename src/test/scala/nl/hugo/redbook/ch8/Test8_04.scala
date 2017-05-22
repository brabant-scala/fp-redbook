package nl.hugo.redbook.ch8

import nl.hugo.redbook.ch6.RNG
import org.scalatest.{ Matchers, WordSpec }

class Test8_04 extends WordSpec with Matchers {
  "A generator" should {
    "have numbers between start and stopExclusive" in {
      for (n <- 1 to 1000) {
        val rng: RNG = RNG.Simple(n)
        val (stop, rng2) = RNG.nonNegativeInt(rng)
        val (start, rng3) = RNG.nonNegativeLessThan(stop)(rng2)

        val (gen, _) = Gen.choose(start, stop).sample.run(rng)
        gen should (be >= start and be < stop)
      }
    }
  }
}
