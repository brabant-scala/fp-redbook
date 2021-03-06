package nl.hugo.redbook.ch11

import org.scalatest.{Matchers, WordSpec}

import nl.hugo.redbook.ch4.Some

import scala.language.postfixOps

import scala.{ Either => _, Option => _, Some => _, Stream => _ }

class Test11_03 extends WordSpec with Matchers {
  "Monad.optionMonad" should {
    "sequence" in {
      Monad.optionMonad.sequence(List(
        Some(1),
        Some(2),
        Some(3)
      )) should be(Some(List(1, 2, 3)))
    }

    "traverse" in {
      Monad.optionMonad.traverse(List(1, 2, 3))(Some(_)) should be(Some(List(1, 2, 3)))
    }
  }
}