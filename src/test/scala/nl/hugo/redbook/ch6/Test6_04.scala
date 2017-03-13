package nl.hugo.redbook.ch6

import nl.hugo.redbook.Spec

class Test6_04 extends Spec {

  implicit val last = CNG.zero

  "RNG.ints" should {

    "return a list with multiple ints" in {
      val rng = CNG(1, 2, 3)
      val (is, r) = RNG.ints(3)(rng)
      is.toSet should be(Set(1, 2, 3))
      r should be(last)
    }

    "return an empty list" in {
      val rng = last
      val (is, r) = RNG.ints(0)(rng)
      is should be(Nil)
      r should be(last)
    }

    "return an empty list when argument is negative" in {
      val rng = last
      val (is, r) = RNG.ints(-100)(rng)
      is should be(Nil)
    }
  }
}
