package nl.hugo.redbook.ch11

import org.scalatest.{Matchers, WordSpec}

import scala.language.postfixOps

class Test11_13 extends WordSpec with Matchers {
  "Monad.OptionMonad" should {
    "flatmap via join and map" in {
      val u = Monad.optionMonad.unit(10)
      val v = Monad.optionMonad.unit(9)

      def f(i: Int): Option[Int] = i match {
        case x if x < 10 => Some(x)
        case _ => None
      }

      Monad.optionMonad.__flatMap(v)(f) should be(Some(9))
      Monad.optionMonad.__flatMap(u)(f) should be(None)
    }
  }
}