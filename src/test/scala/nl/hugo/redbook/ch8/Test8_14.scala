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
      sortedProp.run(100, 1000, rng) should be(Prop.Passed)
    }
  }
}
