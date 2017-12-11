package nl.hugo.redbook.ch11

import org.scalatest.{Matchers, WordSpec}

import scala.language.postfixOps

class Test11_08 extends WordSpec with Matchers {
  "Monad.OptionMonad" should {
    "flatMap via compose" in {
      val u = Monad.optionMonad.unit(10)
      val v = Monad.optionMonad.unit(9)

      def f(i: Int): Option[Int] = i match {
        case x if x < 10 => Some(x)
        case _ => None
      }

      Monad.optionMonad._flatMap(v)(f) should be(Some(9))
      Monad.optionMonad._flatMap(u)(f) should be(None)
    }
  }
}