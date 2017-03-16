package nl.hugo.redbook.ch6

import org.scalatest._

class Test6_08 extends WordSpec with Matchers {
//  val rng = RNG.Simple(0)
//
//  "RNG.flatMap" should {
//    "apply the function" in {
//      val r = RNG.unit(1234)
//
//      RNG.flatMap(r) { (i: Int) => RNG.unit(i + 1) }(rng)._1 should be(1235)
//    }
//
//    "progress the RNG object" in {
//      val (_, rng2) = rng.nextInt
//
//      val r = RNG.unit(1)
//
//      RNG.flatMap(r) { _ => RNG.doubleViaMap }(rng)._2 should be(rng2)
//    }
//  }
//
//  "RNG.nonNegativeLessThan" should {
//    "pass a number in range" in {
//      val v = RNGMock(10, rng)
//
//      RNG.nonNegativeLessThan(20)(v)._1 should be(10)
//    }
//
//    "negate a negative number in range" in {
//      val v = RNGMock(-10, rng)
//
//      RNG.nonNegativeLessThan(20)(v)._1 should be(10)
//    }
//
//    "pass zero" in {
//      val v = RNGMock(0, rng)
//
//      RNG.nonNegativeLessThan(20)(v)._1 should be(0)
//    }
//
//    "return zero when passing the range value" in {
//      val v = RNGMock(20, rng)
//
//      RNG.nonNegativeLessThan(20)(v)._1 should be(0)
//    }
//
//    "take the modulo if a number is out of range" in {
//      val v = RNGMock(30, rng)
//
//      RNG.nonNegativeLessThan(20)(v)._1 should be(10)
//    }
//
//    "progress the RNG object" in {
//      val (_, rng2) = rng.nextInt
//
//      RNG.nonNegativeLessThan(20)(rng)._2 should be(rng2)
//    }
//  }
}
