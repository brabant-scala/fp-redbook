package nl.hugo.redbook.ch6

import nl.hugo.redbook.Spec

class Test6_07 extends Spec {

  implicit val last = CNG.zero

  "sequence" should {

    "combine multiple actions into one action" in {
      val rng = CNG(1, 2, 3)
      val rand = RNG.sequence(List.fill(3)(RNG.int))
      val (l, r) = rand(rng)
      l.toSet should be(Set(1, 2, 3))
      r should be(last)
    }
  }

  "intsViaSequence" should {

    "return a list with random ints" in {
      val rng = CNG(1, 2, 3)
      val rand = RNG.intsViaSequence(3)
      val (l, r) = rand(rng)
      l.toSet should be(Set(1, 2, 3))
      r should be(last)
    }
  }
}