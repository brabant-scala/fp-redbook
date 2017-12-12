package nl.hugo.redbook.ch11

import org.scalatest.{Matchers, WordSpec}

import scala.language.postfixOps
import nl.hugo.redbook.ch4.Some

import scala.{Either => _, Option => _, Some => _, Stream => _}

class Test11_04 extends WordSpec with Matchers {
  "Monad.optionMonad" should {
    "replicateM" in {
      Monad.optionMonad.replicateM(4, Some(1)) should be(Some(List(1, 1, 1, 1)))
    }
  }
}