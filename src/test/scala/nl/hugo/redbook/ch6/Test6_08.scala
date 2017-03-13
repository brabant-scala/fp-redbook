package nl.hugo.redbook.ch6

import nl.hugo.redbook.Spec

class Test6_08 extends Spec {

  implicit val last = CNG.zero

  "flatMap" should {

    "combine two actions into one action" in {
      val rng = CNG(3, 4, 5, 6)
      val rand = RNG.flatMap(RNG.int)(RNG.intsViaSequence)
      val (l, r) = rand(rng)
      l.toSet should be(Set(4, 5, 6))
      r should be(last)
    }
  }

  "nonNegativeLessThan" should {

    "return random non-negative ints that are less than an upper limit" in {
      var rng: RNG = RNG.Simple(System.currentTimeMillis)
      for (_ <- 1 to 1000) {
        val (n, r) = RNG.nonNegativeLessThan(42)(rng)
        rng = r
        n should be >= 0
        n should be < 42
      }
    }

    "return a new generator" in {
      val rng = RNG.Simple(0)
      val (n, r) = RNG.nonNegativeLessThan(42)(rng)
      r should not be (rng)
    }

    "handle corner case" in {
      val rng = CNG(Int.MaxValue, Int.MaxValue - 1, Int.MaxValue - 2, 13)
      val (n, _) = RNG.nonNegativeLessThan(42)(rng)
      n should be(41)
    }
  }
}