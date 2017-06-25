package nl.hugo.redbook.ch9

import scala.util.matching.Regex

object LocationParser {

  // Section 9.6.2
  type Parser[+A] = Location => Result[A]

  // Section 9.6.2
  trait Result[+A] {

    // Section 9.6.5
    def advanceSuccess(chars: Int): Result[A] = this

    // Section 9.6.5
    def addCommit(isCommited: Boolean): Result[A] = this

    // Section 9.6.4
    def uncommit: Result[A] = this

    // Section 9.6.3
    def mapError(f: ParseError => ParseError): Result[A] = this
  }

  // Section 9.6.2
  case class Success[+A](get: A, charsConsumed: Int) extends Result[A] {

    // Section 9.6.5
    override def advanceSuccess(chars: Int): Result[A] =
      copy(charsConsumed = charsConsumed + chars)
  }

  // Section 9.6.2
  case class Failure(get: ParseError, isCommitted: Boolean = false) extends Result[Nothing] {

    // Section 9.6.5
    override def addCommit(isCommited: Boolean): Result[Nothing] =
      Failure(get, this.isCommitted || isCommitted)

    // Section 9.6.4
    override def uncommit: Result[Nothing] =
      copy(isCommitted = false)

    // Section 9.6.3
    override def mapError(f: ParseError => ParseError): Result[Nothing] =
      copy(get = f(get))
  }

  object Impl extends Parsers[Parser] {

    // Exercise 9.13
    def string(s: String): Parser[String] =
      string_2(s)

    // Exercise 9.13
    def string_1(s: String): Parser[String] =
      (location: Location) => {
        val input = location.current
        if (input.startsWith(s))
          Success(s, s.length)
        else
          Failure(Location(input).toError(s"Expected: $s"))
      }

    // Exercise 9.14
    def string_2(s: String): Parser[String] =
      (location: Location) => {
        val input = location.current
        val length = input.zip(s).takeWhile { case (l, r) => l == r }.size
        if (length == s.length)
          Success(s, length)
        else
          Failure(location.advanceBy(length).toError(s"Expected: $s"))
      }

    // Exercise 9.13
    override def succeed[A](a: A): Parser[A] =
      (location: Location) =>
        Success(a, 0)

    // Exercise 9.13
    def regex(r: Regex): Parser[String] =
      (location: Location) => {
        val input = location.current
        r.findFirstIn(input) match {
          case Some(m) if input.startsWith(m) =>
            Success(m, m.length)
          case _ =>
            Failure(Location(input).toError(s"Expected match: ${r.pattern}"))
        }
      }

    // Exercise 9.13
    def slice[A](p: Parser[A]): Parser[String] =
      (location: Location) =>
        p(location) match {
          case Success(_, n) =>
            Success(location.current.take(n), n)
          case failure: Failure =>
            failure
        }

    // Section 9.6.3
    def label[A](message: String)(p: Parser[A]): Parser[A] =
      location => p(location).mapError(_.label(message))

    // Section 9.6.3
    def scope[A](message: String)(p: Parser[A]): Parser[A] =
      location => p(location).mapError(_.push(location, message)) // Error in book on p. 168

    // Section 9.6.4
    def attempt[A](p: Parser[A]): Parser[A] =
      location => p(location).uncommit

    // Section 9.6.4
    def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A] =
      location => p1(location) match {
        case Failure(_, false) => p2(location)
        case result => result
      }

    // Section 9.6.5
    def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] =
      location => p(location) match {
        case Success(a, n) =>
          f(a)(location.advanceBy(n)).addCommit(n != 0).advanceSuccess(n)
        case failure: Failure =>
          failure
      }

    // Exercise 9.15 - end-of-file
    def eof: Parser[Unit] =
      location =>
        if (location.current.isEmpty) Success((), 0)
        else Failure(location.toError(s"Expected end-of-input"))

    // Exercise 9.15
    def run[A](p: Parser[A])(input: String): Either[ParseError, A] = {
      val location = Location(input)
      p(location) match {
        case Success(a, _) => Right(a)
        case failure: Failure => Left(failure.get)
      }
    }

    // Exercise 9.15
    def errorLocation(e: ParseError): Location =
      e.latestLocation.get

    // Exercise 9.15
    def errorMessage(e: ParseError): String =
      e.latest.get._2
  }
}
