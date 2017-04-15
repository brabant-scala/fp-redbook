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

import org.scalatest.{Matchers, WordSpec}

class Test6_04ViaSequence extends WordSpec with Matchers {
  val rng = RNG.Simple(0)

  "RNG.ints" should {
    "convert create list of 3 random integers" in {
      val mock = RNGMock(1, RNGMock(2, RNGMock(3, rng)))

      // DISCUSS: shouldn't the list that we expect be reversed (like in next line)? << No, values come in the order 1,2,3
      RNG.intsViaSequence(3)(mock) should be((List(1, 2, 3).reverse, rng))
    }

    "return an empty list" in {
      RNG.intsViaSequence(0)(rng) should be((List.empty, rng))
    }

    "return an empty list when argument is negative" in {
      RNG.intsViaSequence(-100)(rng) should be((List.empty, rng))
    }
  }
}
