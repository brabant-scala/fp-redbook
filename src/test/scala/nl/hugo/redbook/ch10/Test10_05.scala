package nl.hugo.redbook.ch10

import nl.hugo.redbook.ch10.Monoid._
import org.scalatest.{ Matchers, WordSpec }

class Test10_05 extends WordSpec with Matchers {
  "foldMap" should {
    "take the sum of a list" in {
      val l = List("1", "2", "3", "4", "5")

      foldMap(l, intAddition)(_.toInt) shouldBe 15
    }

    "take the poduct of a list" in {
      val l = List("2", "3", "5", "7")

      foldMap(l, intMultiplication)(_.toInt) shouldBe 210
    }

    val stringMonoid = new Monoid[String] {
      def op(a1: String, a2: String): String = a1 + a2
      def zero = ""
    }

    "correctly concatenate an empty list of chars" in {
      val l = List.empty[Char]

      foldMap(l, stringMonoid)(_.toString) shouldBe ""
    }

    "correctly concatenate a list of chars into a string" in {
      val l = List('a', 'b', 'c', 'd', 'e')

      foldMap(l, stringMonoid)(_.toString) shouldBe "abcde"
    }
  }
}