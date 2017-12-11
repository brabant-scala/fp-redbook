package nl.hugo.redbook.ch12

import org.scalatest.{Matchers, WordSpec}

import scala.language.postfixOps

class Test12_05 extends WordSpec with Matchers {
  "An eitherMonad" should {
    val eitherMonad = Monad.eitherMonad[String]

    "assign a value with unit" in {
      val u = eitherMonad.unit(1)

      u.right.get should be(1)
    }

    "flatMap a value" in {
      val u = eitherMonad.unit(1)

      eitherMonad.flatMap(u)(v => Right(v.toString)).right.get should be("1")
    }

    "flatMap to an error" in {
      val u = eitherMonad.unit(1)

      eitherMonad.flatMap(u)(v => Left(s"Error $v")).left.get should be("Error 1")
    }
  }
}