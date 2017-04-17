package nl.hugo.redbook.ch6

import nl.hugo.redbook.Spec

class Test6_09 extends Spec {

  implicit val last = CNG.zero

  "map (via flatMap)" should {

    "convert an action into another action" in {
      val rng = CNG(42)
      val rand = RNG.map(RNG.int)(_ / 6)
      val (n, r) = rand(rng)
      n should be(7)
      r should be(last)
    }
  }

  "map2 (via flatMap)" should {

    "combine two actions into one action" in {
      val rng = CNG(6, 7)
      val rand = RNG.map2(RNG.int, RNG.int)(_ * _)
      val (n, r) = rand(rng)
      n should be(42)
      r should be(last)
    }
  }
}