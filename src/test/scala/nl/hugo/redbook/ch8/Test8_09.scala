package nl.hugo.redbook.ch8

import nl.hugo.redbook.ch6.RNG
import org.scalatest.{ BeforeAndAfterEach, Matchers, WordSpec }

class Test8_09 extends WordSpec with Matchers with BeforeAndAfterEach {
  var passingPropCount = 0
  val passingProp: Prop = Prop.forAll(Gen.unit("foo"))(
    _ => {
      passingPropCount += 1
      true
    }
  )
  var failingPropCount = 0
  val failingProp: Prop = Prop.forAll(Gen.unit("foo"))(
    _ => {
      failingPropCount += 1
      false
    }
  )

  override def beforeEach(): Unit = {
    passingPropCount = 0
    failingPropCount = 0
  }

  val rng = RNG.Simple(0)

  "Prop.&&" should {
    "pass two passing props" in {
      (passingProp && passingProp).run(0, 1, rng) should be(Prop.Passed)
      passingPropCount should be(2)
      failingPropCount should be(0)
    }

    "fail when the first prop fails and not evaluate the second prop" in {
      (failingProp && passingProp).run(0, 1, rng) should be(Prop.Falsified("foo", 0))
      passingPropCount should be(0)
      failingPropCount should be(1)
    }

    "fail when the second prop fails" in {
      (passingProp && failingProp).run(0, 1, rng) should be(Prop.Falsified("foo", 0))
      passingPropCount should be(1)
      failingPropCount should be(1)
    }

    "fail when both props fail and not evaluate the second prop" in {
      (failingProp && failingProp).run(0, 1, rng) should be(Prop.Falsified("foo", 0))
      passingPropCount should be(0)
      failingPropCount should be(1)
    }
  }

  "Prop.||" should {
    "pass two passing props but evaluate the second prop" in {
      (passingProp || passingProp).run(0, 1, rng) should be(Prop.Passed)
      passingPropCount should be(1)
      failingPropCount should be(0)
    }

    "pass when only the first prop fails" in {
      (failingProp || passingProp).run(0, 1, rng) should be(Prop.Passed)
      passingPropCount should be(1)
      failingPropCount should be(1)
    }

    "pass when only the second prop fails" in {
      (passingProp || failingProp).run(0, 1, rng) should be(Prop.Passed)
      passingPropCount should be(1)
      failingPropCount should be(0)
    }

    "fail when both props fail" in {
      (failingProp || failingProp).run(0, 1, rng) should be(Prop.Falsified("foo", 0))
      passingPropCount should be(0)
      failingPropCount should be(2)
    }
  }

}
