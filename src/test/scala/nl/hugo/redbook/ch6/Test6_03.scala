package nl.hugo.redbook.ch6

import org.scalatest._

class Test6_03 extends WordSpec with Matchers {
  val rng = SimpleRNG(0)

  def toDouble(i: Int): Double = i / (Int.MaxValue.toDouble + 1)

  "RNG.intDouble" should {
    "convert two integers into an int and double" in {
      val mock = RNGMock(1234, RNGMock(5678, rng))

      RNG.intDouble(mock)._1 should be((1234, toDouble(5678)))
    }

    "convert zeros to zeros" in {
      val mock = RNGMock(0, RNGMock(0, rng))

      RNG.intDouble(mock)._1 should be((0, 0.0))
    }

    "return a negative integer value" in {
      val mock = RNGMock(-100, RNGMock(0, rng))

      RNG.intDouble(mock)._1 should be((-100, 0.0))
    }

    "return the next RNG object" in {
      val mock = RNGMock(0, RNGMock(1, rng))

      RNG.intDouble(mock)._2 should be(rng)
    }
  }

  "RNG.doubleInt" should {
    "convert two integers into a double and an integer" in {
      val mock = RNGMock(1234, RNGMock(5678, rng))

      RNG.doubleInt(mock)._1 should be((toDouble(1234), 5678))
    }

    "convert zeros to zeros" in {
      val mock = RNGMock(0, RNGMock(0, rng))

      RNG.doubleInt(mock)._1 should be((0.0, 0))
    }

    "return a negative integer value" in {
      val mock = RNGMock(0, RNGMock(-100, rng))

      RNG.doubleInt(mock)._1 should be((0.0, -100))
    }

    "return the next RNG object" in {
      val mock = RNGMock(0, RNGMock(1, rng))

      RNG.doubleInt(mock)._2 should be(rng)
    }
  }

  "RNG.double3" should {
    "convert three integers into three doubles" in {
      val mock = RNGMock(123, RNGMock(456, RNGMock(789, rng)))

      RNG.double3(mock)._1 should be((toDouble(123), toDouble(456), toDouble(789)))
    }

    "convert zeros to zeros" in {
      val mock = RNGMock(0, RNGMock(0, RNGMock(0, rng)))

      RNG.double3(mock)._1 should be((0.0, 0.0, 0.0))
    }

    "return the next RNG object" in {
      val mock = RNGMock(0, RNGMock(1, RNGMock(2, rng)))

      RNG.double3(mock)._2 should be(rng)
    }
  }
}
