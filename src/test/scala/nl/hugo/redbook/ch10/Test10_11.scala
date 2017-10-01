package nl.hugo.redbook.ch10

import nl.hugo.redbook.ch10.Monoid._
import nl.hugo.redbook.ch8.Gen
import org.scalatest.{ Matchers, WordSpec }

class Test10_11 extends WordSpec with Matchers {
  "Monoid.count" should {

    "count an empty string" in {
      count("") shouldBe 0
    }

    "count a string with just whitespace" in {
      count("      ") shouldBe 0
    }

    "count a string with no whitespace" in {
      count("foxhound") shouldBe 1
    }

    "count a regular string" in {
      count("the quick brown fox jumped over the lazy dog") shouldBe 9
    }

    " count a string surrounded by whitespace " in {
      count("   the quick brown fox jumped over the lazy dog      ") shouldBe 9
    }
  }
}