package nl.hugo.redbook.ch6

import org.scalatest._

class Test6_06 extends WordSpec with Matchers {
  val rng = SimpleRNG(0)

  "RNG.map2" should {
    "apply the function f" in {
      val l = RNG.unit(1234)
      val r = RNG.unit(5678)

      RNG.map2(l, r)(_ + _)(rng) should be((1234 + 5678, rng))
    }

    "progress the RNG object" in {
      val (_, rng2) = rng.nextInt
      val (_, rng3) = rng2.nextInt

      RNG.map2(RNG.double, RNG.double)(_ + _)(rng)._2 should be(rng3)
    }
  }
}
