package nl.hugo.redbook.ch6

import org.scalatest._

class Test6_01 extends WordSpec with Matchers {
  "RNG.nonNegativeInt" should {
    val rng = SimpleRNG(0)
    "return a positive value unmodified" in {
      val mock = RNGMock(42, rng)
      RNG.nonNegativeInt(mock) should be((42, rng))
    }

    "negate a negative value" in {
      val mock = RNGMock(-42, rng)
      RNG.nonNegativeInt(mock) should be((42, rng))
    }

    "return zero unmodified" in {
      val mock = RNGMock(0, rng)
      RNG.nonNegativeInt(mock) should be((0, rng))
    }

    "return the edge condition Int.MinValue as zero" in {
      val mock = RNGMock(Int.MinValue, rng)
      RNG.nonNegativeInt(mock) should be((0, rng))
    }
  }
}
