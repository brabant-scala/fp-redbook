package nl.hugo.redbook.ch11

import org.scalatest.{Matchers, WordSpec}

import scala.language.postfixOps

class Test11_12 extends WordSpec with Matchers {
  "Monad.OptionMonad" should {
    "join" in {
      Monad.optionMonad.join(Some(Some(1))) should be(Some(1))
      Monad.optionMonad.join(Some(None)) should be(None)
      Monad.optionMonad.join(None) should be(None)
    }
  }
}