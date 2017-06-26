package nl.hugo.redbook.ch9

import MyParserTypes._
import org.scalatest.{ Matchers, WordSpec }

class MyParserTypesSpec extends WordSpec with Matchers {
  "firstNonMatchingIndex" should {
    "return none for two empty strings" in {
      firstNonMatchingIndex("", "") should be(None)
    }

    "return none for two identical strings" in {
      firstNonMatchingIndex("foobar", "foobar") should be(None)
    }

    "return none if the first string is a prefix substring of the second string" in {
      firstNonMatchingIndex("foo", "foobar") should be(None)
    }

    "return Some[Int] if the second string is a prefix substring of the first string" in {
      firstNonMatchingIndex("foobar", "foo") should be(Some(3))
    }

    "return Some[Int] if the the two strings have the same prefix" in {
      firstNonMatchingIndex("foobar", "foorab") should be(Some(3))
    }
  }
}
