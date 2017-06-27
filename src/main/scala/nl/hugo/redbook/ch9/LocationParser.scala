package nl.hugo.redbook.ch9

import scala.util.matching.Regex

object LocationParser {

  type Parser[+A] = Location => Result[A]

  trait Result[+A] {
    def advanceSuccess(chars: Int): Result[A] = this match {
      case Success(a, n) => Success(a, chars + n)
      case failure => failure
    }
    def addCommit(isCommitted: Boolean): Result[A] = this match {
      case Failure(e, c) => Failure(e, c || isCommitted)
      case success => success
    }
    def uncommit: Result[A] = this match {
      case Failure(e, c) => Failure(e, false)
      case success => success
    }
    def mapError(f: ParseError => ParseError): Result[A] = this match {
      case Failure(e, c) => Failure(f(e), c)
      case success => success
    }
  }

  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
  case class Failure(get: ParseError, isCommitted: Boolean = false) extends Result[Nothing]

  object Impl extends Parsers[Parser] {

    // Exercise 9.13
    def string(s: String): Parser[String] = ???

    // Exercise 9.14
    def string2(s: String): Parser[String] = ???

    // Exercise 9.13
    override def succeed[A](a: A): Parser[A] = ???

    // Exercise 9.13
    def regex(r: Regex): Parser[String] = ???

    // Exercise 9.13
    def slice[A](p: Parser[A]): Parser[String] = ???

    def label[A](message: String)(p: Parser[A]): Parser[A] =
      location => p(location).mapError(_.label(message))

    def scope[A](message: String)(p: Parser[A]): Parser[A] =
      location => p(location).mapError(_.push(location, message)) // Error in book on p. 168

    def attempt[A](p: Parser[A]): Parser[A] =
      location => p(location).uncommit

    def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A] =
      location => p1(location) match {
        case Failure(_, false) => p2(location)
        case result => result
      }

    def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] =
      location => p(location) match {
        case Success(a, n) =>
          f(a)(location.advanceBy(n)).addCommit(n != 0).advanceSuccess(n)
        case failure: Failure =>
          failure
      }

    // Exercise 9.15
    def run[A](p: Parser[A])(input: String): Either[ParseError, A] = ???

    // Exercise 9.15
    def errorLocation(e: ParseError): Location = ???

    // Exercise 9.15
    def errorMessage(e: ParseError): String = ???
  }
}