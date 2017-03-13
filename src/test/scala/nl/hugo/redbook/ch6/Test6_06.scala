package nl.hugo.redbook.ch6

import nl.hugo.redbook.Spec

class Test6_06 extends Spec {

  implicit val last = CNG.zero

  "map2" should {

    "combine two actions into one action" in {
      val rng = CNG(6, 7)
      val rand = RNG.map2(RNG.int, RNG.int)(_ * _)
      val (n, r) = rand(rng)
      n should be(42)
      r should be(last)
    }
  }
}
