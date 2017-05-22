package nl.hugo.redbook.ch8

import nl.hugo.redbook.ch6.RNG
import org.scalatest.{Matchers, WordSpec}
import org.scalatest.Inspectors._

class Test8_06 extends WordSpec with Matchers {
  "A generator" should {
    "flatMap another Generator" in {
      val chooser = Gen.choose(20, 30)
      val gen = chooser.flatMap(n => Gen.listOfN(n, Gen.boolean))
      for (n <- 0 to 1000) {
        val rng: RNG = RNG.Simple(n)
        val (len, _) = chooser.sample.run(rng)
        val (lst, _) = gen.sample.run(rng)

        lst.distinct.length should be > 1
        lst.length should be(len)
      }
    }

    "produce a listOfN" in {
      val gen = Gen.choose(20, 30).listOfN(10)
      for (n <- 0 to 1000) {
        val (lst, _) = gen.sample.run(RNG.Simple(n))
        lst.size should be(10)
        lst.distinct.size should be > 1
        forAll(lst) { x => x should (be >= 20 and be < 30) }
      }
    }
  }
}
