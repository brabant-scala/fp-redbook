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

import org.scalatest.{Matchers, WordSpec}

class Test6_01ViaMap extends WordSpec with Matchers {
  "RNG.nonNegativeInt" should {
    val rng = RNG.Simple(0)
    "return a positive value unmodified" in {
      val mock = RNGMock(42, rng)
      RNG.nonNegativeIntViaMap(mock) should be((42, rng))
    }

    "negate a negative value" in {
      val mock = RNGMock(-42, rng)
      RNG.nonNegativeIntViaMap(mock) should be((42, rng))
    }

    "return zero unmodified" in {
      val mock = RNGMock(0, rng)
      RNG.nonNegativeIntViaMap(mock) should be((0, rng))
    }

    "return the edge condition Int.MinValue as zero" in {
      val mock = RNGMock(Int.MinValue, rng)
      RNG.nonNegativeIntViaMap(mock) should be((0, rng))
    }
  }
}
