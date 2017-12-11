package nl.hugo.redbook.ch8

import nl.hugo.redbook.ch6.RNG
import org.scalatest.{ Matchers, WordSpec }
import Prop._

class Test8_14 extends WordSpec with Matchers {
  "Gen.sortedProp" should {
    "should pass" in {
      val rng = RNG.Simple(System.nanoTime())

      // Exercise 8.14
      val smallInt = Gen.choose(-10, 10)
      val sortedProp = Prop.forAll(Gen.listOf(smallInt)) { ns =>
        val sorted = ns.sorted
        // TODO: check multiplicity
        (sorted.size == ns.size) &&
          sorted.forall(ns.contains) &&
          (sorted.isEmpty || sorted.zip(sorted.tail).forall { case (l, r) => l <= r }) // TODO: use sliding?
      }

      sortedProp.run(100, 1000, rng) should be(Prop.Passed)
    }
  }
}
