package nl.hugo.redbook.ch9

import nl.hugo.redbook.ch9.MyParser._
import nl.hugo.redbook.ch9.MyParserTypes._
import org.scalatest.{ Matchers, WordSpec }

class MyParserSpec extends WordSpec with Matchers {
  "MyParser.string" should {
    val p = string("foo")
    "match a prefix string" in {
      p(Location("foobar")) should be(Success("foo", 3))
    }

    "reject a prefix" in {
      val loc = Location("zfoobar")
      p(loc) should be(a[Failure])
    }

    "match a string at an offset position" in {
      p(Location("0123foo789", 3))
    }

    "reject a string at a prefix position" in {
      val loc = Location("0123456789", 3)
      p(loc) should be(a[Failure])
    }
  }

  "MyParser.slice" should {
    val p = string("a").many.slice
    "slice many parsers" in {
      val loc = Location("aaaaaab")

      p(loc) should be(Success("aaaaaa", 6))
    }
  }

  "MyParser.flatMap" should {
    "process" in {
      val loc = Location("3bbb")

      val q = (_: String) => string("b")

      int.flatMap(string("b").listOfN(_))(loc) should be(Success(List("b", "b", "b"), 4))
    }
  }

  "MyParser.quoted" should {
    "process letters" in {
      val loc = Location(""""abcdefghijklmnopqrstuvwxyz"""")

      quoted(loc) should be( Success("abcdefghijklmnopqrstuvwxyz", 28))
    }

    "process number" in {
      val loc = Location(""""0123456789"""")

      quoted(loc) should be( Success("0123456789", 12))
      quoted(Location(""""6" """)) should be( Success("6", 3))
    }
  }

  "MyParser.double" should {
    "fail on a quoted number" in {
      val loc = Location(""""62"""")

      double(loc) should be (a[Failure])
    }
  }

  "MyParser.doubleString" should {
    "fail on a quoted number" in {
      val loc = Location(""""62"""")

      doubleString(loc) should be (a[Failure])
    }
  }
}
