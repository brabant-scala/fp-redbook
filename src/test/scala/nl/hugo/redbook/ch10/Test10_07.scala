package nl.hugo.redbook.ch10

import nl.hugo.redbook.ch10.Monoid._
import org.scalatest.{ Matchers, WordSpec }
import Helpers._

class Test10_07 extends WordSpec with Matchers {
  "foldMapV" should {

    "correctly concatenate an empty list of chars" in {
      val l = List.empty[Char]

      foldMap(l, stringMonoid)(_.toString) shouldBe ""
    }

    "correctly concatenate a list of chars into a string" in {
      val l = List(
        'a', 'b', 'c', 'd', 'e', 'f',
        'g', 'h', 'i', 'j', 'k', 'l',
        'm', 'n', 'o', 'p', 'q', 'r',
        's', 't', 'u', 'v', 'w', 'x',
        'y', 'z'
      )

      foldMap(l, stringMonoid)(_.toString) shouldBe "abcdefghijklmnopqrstuvwxyz"
    }
  }
}