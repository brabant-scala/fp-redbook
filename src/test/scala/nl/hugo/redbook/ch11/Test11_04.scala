package nl.hugo.redbook.ch11

import org.scalatest.{Matchers, WordSpec}

import scala.language.postfixOps


class Test11_04 extends WordSpec with Matchers {
  "Monad.optionMonad" should {
    "replicateM" in {
      Monad.optionMonad.replicateM(4, Some(1)) should be(Some(List(1, 1, 1, 1)))
    }
  }
}