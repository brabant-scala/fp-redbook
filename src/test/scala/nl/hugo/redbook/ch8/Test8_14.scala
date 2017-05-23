package nl.hugo.redbook.ch8

import nl.hugo.redbook.ch6.RNG
import org.scalatest.{Matchers, WordSpec}
import Prop._

class Test8_14 extends WordSpec with Matchers {
  "Gen.sortedProp" should {
    "should pass" in {
      val rng = RNG.Simple(System.nanoTime())

      // Exercise 8.14
      // val sortedProp: Prop = ???
      val smallInt = Gen.choose(-10,10)
      val sortedProp = Prop.forAll(Gen.listOf(smallInt)) { ns =>
        val nss = ns.sorted
        // We specify that every sorted list is either empty, has one element,
        // or has no two consecutive elements `(a,b)` such that `a` is greater than `b`.
        nss.isEmpty || nss.tail.isEmpty || !nss.zip(nss.tail).exists {
          case (a,b) => a > b
        } &&
        // Also, the sorted list should have all the elements of the input list,
        !ns.exists(!nss.contains(_)) &&
        // and it should have no elements not in the input list.
        !nss.exists(!ns.contains(_))
      }

      sortedProp.run(100, 1000, rng) should be(Prop.Passed)
    }
  }
}
