package nl.hugo.redbook.ch10

import nl.hugo.redbook.ch10.Monoid._
import nl.hugo.redbook.ch6.RNG
import nl.hugo.redbook.ch8.Prop.{ Falsified, Passed, Proved }
import nl.hugo.redbook.ch8._
import org.scalatest.{ Matchers, WordSpec }

class Test10_06 extends WordSpec with Matchers {

  "foldRight" should {
    val l = List('a', 'b', 'c', 'd', 'e')
    "foldRight a list of chars" in {
      def append(c: Char, s: String): String = s :+ c

      foldRight(l)("S")(append) shouldBe "Sedcba"
    }

    "foldLeft a list of chars" in {
      def append(s: String, c: Char): String = s :+ c
      foldLeft(l)("S")(append) shouldBe "Sabcde"
    }
  }
}