package nl.hugo.redbook.ch8

import org.scalatest.{ Matchers, WordSpec }

class Test8_03 extends WordSpec with Matchers {
  case object FailingBooleanProp extends BooleanProp {
    override def check: Boolean = false
  }

  case object SuccessfulBooleanProp extends BooleanProp {
    override def check: Boolean = true
  }

  "A successful prop combined with a sucessful prop" should {
    "succeed" in {
      (SuccessfulBooleanProp && SuccessfulBooleanProp).check shouldBe true
    }
  }

  "A successful prop combined with a failing prop" should {
    "fail" in {
      (SuccessfulBooleanProp && FailingBooleanProp).check shouldBe false
    }
  }

  "A failing prop combined with a sucessful prop" should {
    "fail" in {
      (FailingBooleanProp && SuccessfulBooleanProp).check shouldBe false
    }
  }

  "A failing prop combined with a failing prop" should {
    "fail" in {
      (FailingBooleanProp && FailingBooleanProp).check shouldBe false
    }
  }

}