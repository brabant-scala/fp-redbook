package nl.hugo.redbook.ch11

import nl.hugo.redbook.ch4.{None, Some, Option}
import org.scalatest.{Matchers, WordSpec}

import scala.language.postfixOps
import scala.{Either => _, Option => _, Some => _, Stream => _}

class Test11_07 extends WordSpec with Matchers {
  "Monad.optionMonad" should {
    "compose" in {
      def f(i: Int): Option[String] = Monad.optionMonad.unit(i.toString)
      def g(s: String): Option[Int] = Monad.optionMonad.unit(s.length)

      Monad.optionMonad.compose(f,g)(10000) should be(Some(5))
      Monad.optionMonad.compose((i:Int) => None, g)(10000) should be(None)
      Monad.optionMonad.compose(f, (s: String) => None)(10000) should be(None)
    }
  }
}