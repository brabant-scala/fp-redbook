package nl.hugo.redbook.ch8

import nl.hugo.redbook.ch6.RNG
import org.scalatest.{Matchers, WordSpec}

class Test8_08 extends WordSpec with Matchers {
  (0.0 to 1.0 by 0.1).foreach { pl =>
    s"An weighted pair of generators with weights($pl, ${1 - pl})" should {
      val pr = 1.0 - pl
      val union = Gen.weighted((Gen.unit(0.0), pl), (Gen.unit(1.0), pr))

      // The union is a binomial distribution with p = 0.5
      val n = 2000

      val sampler = union.listOfN(n)

      val expectedMean = pr
      val expectedVariance = pl * pr

      val rng = RNG.Simple(0)

      val (samples, _) = sampler.sample.run(rng)

      val mean = samples.sum / n

      "have the expected mean" in {
        mean should be(expectedMean +- 0.025)
      }

      "have the expected variance" in {
        val variance = samples.map(v => Math.pow(v - mean, 2)).sum / n
        variance should be(expectedVariance +- 0.0125)
      }
    }
  }

  "An weighted Gen" should {
    def generate[A](g: Gen[A]) = (s: Long) => g.listOfN(4).sample.run(RNG.Simple(s))._1

    def randomListsFrom[A](g: Gen[A]) = (0l to 25l).map(generate(g))

    "when left biased only contain left elements" in {
      val gen = Gen.weighted((Gen.unit(true), 1.0), (Gen.unit(false), 0.0))
      randomListsFrom(gen) should contain only List(true, true, true, true)
    }

    "when right biased only contain right elements" in {
      val gen = Gen.weighted((Gen.unit(true), 0.0), (Gen.unit(false), 1.0))
      randomListsFrom(gen) should contain only List(false, false, false, false)
    }

    "not simply alternate between generators" in {
      val gen = Gen.weighted((Gen.unit(true), 0.5), (Gen.unit(false), 0.5))

      randomListsFrom(gen) should not(
        contain only List(false, true, false, true) or
          contain only List(true, false, true, false)
      )
    }

  }
}
