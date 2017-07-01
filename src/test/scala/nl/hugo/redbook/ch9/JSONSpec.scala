package nl.hugo.redbook.ch9

import nl.hugo.redbook.ch9.JSON._
import org.scalacheck.{ Gen, Prop }
import org.scalatest.prop.Checkers
import org.scalatest.{ Matchers, WordSpec }

class JSONSpec extends WordSpec with Matchers with Checkers {
  "valueParser" should {

    val valueParser = JSON.valueParser(MyParser)
    "parse a null" in {
      MyParser.run(valueParser)("null") should be(Right(JNull))
    }

    "parse a number" in {
      check(Prop.forAll { (d: Double) =>
        MyParser.run(valueParser)(d.toString) == Right(JNumber(d))
      })
    }

    "parse a quoted string" in {
      check(Prop.forAll(Gen.alphaStr) {
        (s: String) =>
          MyParser.run(valueParser)(s""""$s"""") == Right(JString(s))
      })
    }

    "parse quoted numbers as a string" in {
      check(Prop.forAll(Gen.numStr.suchThat(!_.isEmpty)) {
        (s: String) =>
          MyParser.run(valueParser)(s""""$s"""") == Right(JString(s))
      })
    }

    "parse a true" in {
      MyParser.run(valueParser)("true") should be(Right(JBool(true)))
    }

    "parse a false" in {
      MyParser.run(valueParser)("false") should be(Right(JBool(false)))
    }

  }
}
