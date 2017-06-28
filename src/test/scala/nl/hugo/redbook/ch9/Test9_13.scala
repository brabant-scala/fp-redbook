package nl.hugo.redbook.ch9

import nl.hugo.redbook.Spec

class Test9_13 extends Spec {

  import MyParser._
  import MyParserTypes._

  def parser: Parser[JSON] = JSON.jsonParser(MyParser)

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
      MyParser.regex(regexpr)(location) should be(Success(token, token.length))
    }

    "not parse a partial regular expression token" in {
      val location = Location(token.take(5))
      val error = ParseError(Nil).push(location, s"Expected match: ${regexpr.pattern}")
      MyParser.regex(regexpr)(location) should be(Failure(error, isCommitted = false))
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