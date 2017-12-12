package nl.hugo.redbook.ch11

import org.scalatest.{Matchers, WordSpec}

import scala.language.postfixOps

class Test11_02 extends WordSpec with Matchers {
  "Monad.stateMonad" should {
    "assign a unit value" in {
      Monad.stateMonad[Int].unit("foo").run(1138) should be((1138, "foo"))
    }

    "flatMap a function" in {
      val u = Monad.stateMonad[Int].unit("foo")

      def d(i: String) = Monad.stateMonad[Int].unit(i + i)

      Monad.stateMonad.flatMap(u)(d).run(1138) should be(("foofoo", 1138))
    }
  }
}