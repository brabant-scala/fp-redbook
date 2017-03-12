package nl.hugo.redbook.ch6

import org.scalatest._

class Test6_04 extends WordSpec with Matchers {
  val rng = RNG.Simple(0)

  "RNG.ints" should {
    "convert create list of 3 random integers" in {
      val mock = RNGMock(1, RNGMock(2, RNGMock(3, rng)))

      RNG.ints(3)(mock) should be((List(1, 2, 3), rng))
      // DISCUSS: shouldn't the list that we expect be reversed (like in next line)?
      RNG.intsViaSequence(3)(mock) should be((List(1, 2, 3).reverse, rng))
    }

    "return an empty list" in {
      RNG.ints(0)(rng) should be((List.empty, rng))
      RNG.intsViaSequence(0)(rng) should be((List.empty, rng))
    }

    "return an empty list when argument is negative" in {
      RNG.ints(-100)(rng) should be((List.empty, rng))
      RNG.intsViaSequence(-100)(rng) should be((List.empty, rng))
    }
  }
}
