package nl.hugo.redbook.ch6

import nl.hugo.redbook.Spec

class Test6_03 extends Spec {

  implicit val last = CNG.zero

  "intDouble" should {

    "return random int-double tuples" in {
      var rng: RNG = RNG.Simple(System.currentTimeMillis)
      for (_ <- 1 to 1000) {
        val ((n, d), r) = RNG.intDouble(rng)
        rng = r
        d should be >= 0.0
        d should be < 1.0
      }
    }

    "return a new generator" in {
      val rng = CNG(0)
      val (_, r) = RNG.intDouble(rng)
      r should be(last)
    }

    "not use the same generator twice" in {
      val rng = CNG(7, 6)(CNG.zero)
      val (_, r) = RNG.intDouble(rng)
      r should be(CNG.zero)
    }
  }

  "doubleInt" should {

    "return random double-int tuples" in {
      var rng: RNG = RNG.Simple(System.currentTimeMillis)
      for (_ <- 1 to 1000) {
        val ((d, n), r) = RNG.doubleInt(rng)
        rng = r
        d should be >= 0.0
        d should be < 1.0
      }
    }

    "return a new generator" in {
      val rng = CNG(0)
      val (_, r) = RNG.doubleInt(rng)
      r should be(last)
    }

    "not use the same generator twice" in {
      val rng = CNG(7, 6)
      val (_, r) = RNG.doubleInt(rng)
      r should be(last)
    }
  }

  "double3" should {

    "return random tuples with 3 doubles" in {
      var rng: RNG = RNG.Simple(0)
      for (_ <- 1 to 1000) {
        val ((d1, d2, d3), r) = RNG.double3(rng)
        rng = r
        d1 should be >= 0.0
        d1 should be < 1.0
        d2 should be >= 0.0
        d2 should be < 1.0
        d3 should be >= 0.0
        d3 should be < 1.0

        // true for the chosen seed
        d1 should not be (d2)
        d1 should not be (d3)
        d2 should not be (d3)
      }
    }

    "return a new generator" in {
      val rng = CNG(0)
      val (_, r) = RNG.double3(rng)
      r should be(last)
    }

    "not use the same generator multiple times" in {
      val rng = CNG(6, 7, 42)
      val (_, r) = RNG.double3(rng)
      r should be(last)
    }
  }
}