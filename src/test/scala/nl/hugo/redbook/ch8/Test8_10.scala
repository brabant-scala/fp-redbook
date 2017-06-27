package nl.hugo.redbook.ch8

import nl.hugo.redbook.ch6.RNG
import org.scalatest.{ BeforeAndAfterEach, Matchers, WordSpec }

class Test8_10 extends WordSpec with Matchers {
  "Gen.unsized" should {
    "return the correct value" in {
      val positiveInts = Gen.choose(0, Int.MaxValue)

      val p = Prop.forAll(positiveInts)(i => Gen.unit("foo").unsized.forSize(i).sample.run(RNG.Simple(0))._1 == "foo")

      p.run(0, 100, RNG.Simple(System.nanoTime())) should be(Prop.Passed)

    }
  }
}
