package nl.hugo.redbook.ch6

import org.scalatest._

class Test6_02 extends WordSpec with Matchers {
  "RNG.double" should {
    val rng = SimpleRNG(0)

    "convert an integer to a double" in {
      val mock = RNGMock(1234, rng)

      RNG.double(mock)._1 should be(0.000000574626028537750244140625 +- 1e-12)
    }

    "convert zero to a zero" in {
      val mock = RNGMock(0, rng)

      RNG.double(mock)._1 should be(0.0)
    }

    "convert IntMaxValue to a double" in {
      val mock = RNGMock(Int.MaxValue, rng)

      RNG.double(mock)._1 should be(0.9999999995343387 +- 1e-12)
    }

    "return the next RNG object" in {
      val mock = RNGMock(0, rng)

      RNG.double(mock)._2 should be(rng)
    }
  }
}
