package nl.hugo.redbook.ch6

import nl.hugo.redbook.Spec

class Test6_01 extends Spec {

  implicit val last = CNG.zero

  "nonNegativeInt" should {

    "return random non-negative ints" in {
      var rng: RNG = RNG.Simple(System.currentTimeMillis)
      for (_ <- 1 to 1000) {
        val (n, r) = RNG.nonNegativeInt(rng)
        rng = r
        n should be >= 0
      }
    }

    "return a new generator" in {
      val rng = CNG(0)
      val (_, r) = RNG.nonNegativeInt(rng)
      r should be(last)
    }

    "handle corner case" in {
      val rng = CNG(Int.MaxValue)
      val (n, _) = RNG.nonNegativeInt(rng)
      n should be >= 0
    }
  }
}