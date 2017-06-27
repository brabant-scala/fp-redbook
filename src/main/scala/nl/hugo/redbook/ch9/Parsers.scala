package nl.hugo.redbook.ch9

import language.{ higherKinds, implicitConversions }
import scala.util.matching.Regex
import nl.hugo.redbook.ch8.{ Gen, Prop }

trait Parsers[Parser[+_]] { self => // so inner classes may call methods of trait

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  implicit def string(s: String): Parser[String]

  def char(c: Char): Parser[Char] =
    string(c.toString).map(_.charAt(0))

  def succeed[A](a: A): Parser[A] =
    string("").map(_ => a)

  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]

  def slice[A](p: Parser[A]): Parser[String]

  // Exercise 9.1
  def productUsingMap2[A, B](pa: Parser[A], pb: => Parser[B]): Parser[(A, B)] = ???

  // Exercise 9.1
  def map2UsingProduct[A, B, C](pa: Parser[A], pb: => Parser[B])(f: (A, B) => C): Parser[C] = ???

  // Exercise 9.2
  def many1[A](p: Parser[A]): Parser[List[A]] = ???

  // Exercise 9.3
  def many[A](p: Parser[A]): Parser[List[A]] = ???

  // Exercise 9.4
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = ???

  // Exercise 9.5
  def nonStrict[A](p: => Parser[A]): Parser[A] = ???

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def regex(r: Regex): Parser[String]

  // Exercise 9.7 - using flatMap
  def product[A, B](pa: Parser[A], pb: => Parser[B]): Parser[(A, B)] = ???

  // Exercise 9.7
  def map2[A, B, C](pa: Parser[A], pb: => Parser[B])(f: (A, B) => C): Parser[C] = ???

  // Exercise 9.8
  def map[A, B](p: Parser[A])(f: A => B): Parser[B] = ???

  def label[A](msg: String)(p: Parser[A]): Parser[A]

  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  def attempt[A](p: Parser[A]): Parser[A]

  def errorLocation(e: ParseError): Location

  def errorMessage(e: ParseError): String

  implicit class ParserOps[A](pa: Parser[A]) {
    def map[B](f: A => B): Parser[B] = self.map(pa)(f)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(pa)(f)
    def slice = self.slice(pa)
    def many = self.many(pa)

    def |[B >: A](pb: => Parser[B]): Parser[B] = self.or(pa, pb)
    def or[B >: A](pb: => Parser[B]): Parser[B] = self.or(pa, pb)

    def **[B](pb: => Parser[B]): Parser[(A, B)] = self.product(pa, pb)
    def product[B](pb: => Parser[B]): Parser[(A, B)] = self.product(pa, pb)
  }

  object Laws {
    import Prop.forAll

    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(identity))(in)
  }
}

case class Location(input: String, offset: Int = 0) {
  private lazy val sliced = input.slice(0, offset + 1)
  lazy val line = sliced.count(_ == '\n') + 1
  lazy val col = sliced.reverse.indexOf('\n')
  /* Section 9.5.1 lists a different implementation:
  lazy val col = sliced.lastIndexOf('\n') match {
    case -1 => offset + 1
    case lineStart => offset - lineStart
  }
  */

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) =
    copy(offset = offset + n)

  /* Returns the line corresponding to this location */
  def currentLine: String =
    if (input.length > 1) input.lines.drop(line - 1).next
    else ""
}

case class ParseError(stack: List[(Location, String)] = Nil) {

  def label[A](s: String): ParseError =
    ParseError(latestLocation.map((_, s)).toList)

  def push(location: Location, message: String): ParseError =
    copy(stack = (location, message) :: stack)

  def latestLocation: Option[Location] =
    latest.map(_._1)

  def latest: Option[(Location, String)] =
    stack.lastOption

  // Exercise 9.16
  override def toString: String = ???
}