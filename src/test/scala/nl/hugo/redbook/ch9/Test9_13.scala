package nl.hugo.redbook.ch9

import nl.hugo.redbook.Spec

class Test9_13 extends Spec {
  import LocationParser._

  def parser = JSON.jsonParser(Impl)

  val token = "Hello, world!"

  "string" should {

    "parse a string token" in {
      val location = Location(token)
      Impl.string(token)(location) should be(Success(token, token.length))
    }

    "not parse a partial string token" in {
      val location = Location(token.take(5))
      val error = ParseError(Nil).push(location.advanceBy(5), s"Expected: $token")
      Impl.string(token)(location) should be(Failure(error, isCommitted = false))
    }
  }

  "regex" should {

    val regex = ".*ll.*!".r

    "parse a regular expression" in {
      val location = Location(token)
      Impl.regex(regex)(location) should be(Success(token, token.length))
    }

    "not parse a partial regular expression token" in {
      val location = Location(token.take(5))
      val error = ParseError(Nil).push(location, s"Expected match: ${regex.pattern}")
      Impl.regex(regex)(location) should be(Failure(error, isCommitted = false))
    }
  }

  "succeed" should {

    "parse always" in {
      val location = Location(token)
      Impl.succeed(42)(location) should be(Success(42, 0))
    }
  }

  "slice" should {

    "return the parsed string" in {
      val location = Location(token)
      Impl.slice(Impl.string(token))(location) should be(Success(token, token.length))
      Impl.slice(Impl.succeed(42))(location) should be(Success("", 0))
    }
  }
}