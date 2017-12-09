package nl.hugo.redbook.ch9

import org.scalatest.{ Matchers, WordSpec }

class Test9_13 extends WordSpec with Matchers {

  import LocationParser._
  import MyParserTypes._

  def parser: Parser[JSON] = JSON.jsonParser(LocationParser)

  val token: String = "Hello, world!"

  "string" should {

    "parse a string token" in {
      val location = Location(token)
      string(token)(location) should be(Success(token, token.length))
    }

    "not parse a partial string token" in {
      val location = Location(token.take(5))
      val error = ParseError(Nil).push(location /*.advanceBy(5)*/ , s"Expected: $token")
      string(token)(location) should be(Failure(error, isCommitted = false))
    }
  }

  "regex" should {

    val regexpr = ".*ll.*!".r

    "parse a regular expression" in {
      val location = Location(token)
      LocationParser.regex(regexpr)(location) should be(Success(token, token.length))
    }

    "not parse a partial regular expression token" in {
      val location = Location(token.take(5))
      val error = ParseError(Nil).push(location, s"Expected match: ${regexpr.pattern}")
      LocationParser.regex(regexpr)(location) should be(Failure(error, isCommitted = false))
    }
  }

  "succeed" should {

    "parse always" in {
      val location = Location(token)
      succeed(42)(location) should be(Success(42, 0))
    }
  }

  "slice" should {

    "return the parsed string" in {
      val location = Location(token)
      slice(string(token))(location) should be(Success(token, token.length))
      slice(succeed(42))(location) should be(Success("", 0))
    }
  }
}