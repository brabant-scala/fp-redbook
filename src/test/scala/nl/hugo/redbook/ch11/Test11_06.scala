package nl.hugo.redbook.ch11

import nl.hugo.redbook.ch4.Some
import nl.hugo.redbook.ch4.None
import org.scalatest.{Matchers, WordSpec}

import scala.language.postfixOps
import scala.{Either => _, Option => _, Some => _, Stream => _}

class Test11_06 extends WordSpec with Matchers {
  "Monad.optionMonad" should {
    "filterM" in {
      Monad.optionMonad.filterM(List(1,2,3))(i => Some(i > 1)) should be(Some(List(2,3)))
      Monad.optionMonad.filterM(List(1,2,3))(i => None) should be(None)
    }
  }
}