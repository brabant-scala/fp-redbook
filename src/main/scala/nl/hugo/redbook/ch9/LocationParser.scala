package nl.hugo.redbook.ch9

import nl.hugo.redbook.ch9.MyParserTypes.{ Failure, Parser, Success }

import scala.util.matching.Regex

object LocationParser extends Parsers[Parser] {

  // Exercise 9.13
  override def string(s: String): Parser[String] =
    (input: Location) =>
      if (input.startsWith(s)) {
        Success(s, s.length)
      } else
        Failure(input.toError(s"Expected: $s"), isCommitted = false)

  // Exercise 9.13
  override def regex(r: Regex): Parser[String] =
    (input: Location) =>
      r.findPrefixOf(input.current) match {
        case Some(s) => Success(s, s.length)
        case None => Failure(input.toError(s"Expected match: $r"), isCommitted = false)
      }

  // Exercise 9.13
  override def succeed[A](a: A): Parser[A] = _ => Success(a, 0)

  // Exercise 9.13
  override def slice[A](p: Parser[A]): Parser[String] =
    (input: Location) =>
      p(input) match {
        case Success(_, n) => Success(input.slice(n), n)
        case f @ Failure(_, _) => f
      }

  override def attempt[A](p: Parser[A]): Parser[A] =
    s => p(s).uncommit

  override def flatMap[A, B](p: Parser[A])(g: (A) => Parser[B]): Parser[B] =
    s => p(s) match {
      case Success(v, n) => g(v)(s.advanceBy(n))
        .addCommit(n != 0)
        .advanceSuccess(n)
      case f @ Failure(_, _) => f
    }

  override def label[A](msg: String)(p: Parser[A]): Parser[A] =
    s => p(s).mapError(_.label(msg))

  override def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A] =
    s => s1(s) match {
      case Failure(e, false) => s2(s)
      case r => r
    }

  override def run[A](p: Parser[A])(input: String): Either[ParseError, A] =
    p(Location(input)) match {
      case Success(v, _) => Right(v)
      case Failure(m, _) => Left(m)
    }

  override def scope[A](msg: String)(p: Parser[A]): Parser[A] =
    s => p(s).mapError(_.push(s, msg))
}