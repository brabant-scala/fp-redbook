package nl.hugo.redbook.ch11

import nl.hugo.redbook.ch4.{None, Option, Some}
import org.scalatest.{Matchers, WordSpec}

import scala.language.postfixOps
import scala.{Either => _, Option => _, Some => _, Stream => _}

class Test11_17 extends WordSpec with Matchers {
  "Id" should {
    "map" in {
      Id(1).map(_.toString) should be(Id("1"))
    }

    "flatMap" in {
      Id(1).flatMap(i => Id(i.toString)) should be(Id("1"))
    }
  }

  "Monad.idMonad" should {
    "assign a unit value" in {
      Monad.idMonad.unit(10) shouldBe Id(10)
    }

    "flatMap a function" in {
      val u = Monad.idMonad.unit(10)
      val v = Monad.idMonad.unit(9)

      def f(i: Int): Id[Int] = Id(i + 10)

      Monad.idMonad.flatMap(v)(f) should be(Id(19))
      Monad.idMonad.flatMap(u)(f) should be(Id(20))
    }
  }
}