package nl.hugo.redbook.ch8

import nl.hugo.redbook.ch6.RNG
import org.scalatest.Inspectors._
import org.scalatest.{Matchers, WordSpec}

class Test8_07 extends WordSpec with Matchers {
  "An union" should {
    "should sample take values from the two generators equally" in {
      val union = Gen.union(Gen.unit(0.0), Gen.unit(1.0))

      // The union is a binomial distribution with p = 0.5
      val n = 2000
      val p = 0.5

      val sampler = union.listOfN(n)

      val expectedMean = p
      val expectedVariance = p * (1 - p)

      val rng = RNG.Simple(0)

      val (samples, _) = sampler.sample.run(rng)

      val mean = samples.sum / n
      mean should be(expectedMean +- 0.025)
      samples.map(v => Math.pow(v - mean, 2)).sum / n should be(expectedVariance +- 0.0125)
    }
  }

  "An union of bools" should {
    val boolUnionGen = Gen.union(Gen.unit(true), Gen.unit(false))

    val listGen = boolUnionGen.listOfN(4)

    def generateList = (s: Long) => listGen.sample.run(RNG.Simple(s))._1

    val randomLists = (0l to 25l).map(generateList)

    "not produce all values from the first generator" in {
      randomLists should not contain only(List(true, true, true, true))
    }

    "not produce all values from the second generator" in {
      randomLists should not contain only(List(false, false, false, false))
    }

    "not simply alternate between generators" in {
      randomLists should not (
        contain only List(false, true, false, true) or
          contain only List(true, false, true, false)
      )
    }
  }
}
