package nl.hugo.redbook.ch9

import language.{ higherKinds, implicitConversions }
import scala.util.matching.Regex
import nl.hugo.redbook.ch8.{ Gen, Prop, SGen }

// Section 9.1
trait Parsers[Parser[+_]] { self =>

  // Section 9.1
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  // Section 9.1
  implicit def string(s: String): Parser[String]

  // Section 9.2
  def char(c: Char): Parser[Char] =
    string(c.toString).map(_.charAt(0))

  // Section 9.2
  def succeed[A](a: A): Parser[A] =
    string("").map(_ => a)

  // Section 9.1
  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]

  // Section 9.2.1
  def slice[A](p: Parser[A]): Parser[String]

  // Exercise 9.1 - using map2
  def productUsingMap2[A, B](pa: Parser[A], pb: => Parser[B]): Parser[(A, B)] =
    map2(pa, pb)((a, b) => (a, b))

  // Exercise 9.1
  def map2UsingProduct[A, B, C](pa: Parser[A], pb: => Parser[B])(f: (A, B) => C): Parser[C] =
    (pa ** pb).map(f.tupled)

  // Exercise 9.2 - one or more (+)
  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _)

  // Exercise 9.3
  def many[A](p: Parser[A]): Parser[List[A]] =
    many1(p) | succeed(Nil)

  // Exercise 9.4
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n > 0) map2(p, listOfN(n - 1, p))(_ :: _)
    else succeed(Nil)

  // Exercise 9.5
  def nonStrict[A](p: => Parser[A]): Parser[A] =
    p

  // Sectiom 9.3
  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  // Section 9.3
  def regex(r: Regex): Parser[String]

  // Exercise 9.7 - using flatMap
  def product[A, B](pa: Parser[A], pb: => Parser[B]): Parser[(A, B)] =
    flatMap(pa)(a => pb.map(b => (a, b)))

  // Exercise 9.7
  def map2[A, B, C](pa: Parser[A], pb: => Parser[B])(f: (A, B) => C): Parser[C] =
    flatMap(pa)(a => pb.map(b => f(a, b)))

  // Exercise 9.8
  def map[A, B](p: Parser[A])(f: A => B): Parser[B] =
    flatMap(p)(a => succeed(f(a)))

  // Exercise 9.9 - within
  def within[A, B, C](pb: Parser[B], pc: Parser[C])(pa: Parser[A]): Parser[A] =
    pb -* pa *- pc

  // Exercise 9.9 - delimited sequence
  def delimited[A, B](pb: Parser[B])(pa: Parser[A]): Parser[List[A]] =
    (pa ** (pb -* pa).many).map { case (a, as) => a :: as } | succeed(Nil)

  // Exercise 9.9 - optional
  def option[A](pa: Parser[A]): Parser[Option[A]] =
    pa.map(Some(_)) | succeed(None)

  // Exercise 9.9 - multiple ors
  def oneOf[A](p: Parser[A], ps: Parser[A]*): Parser[A] =
    ps.fold(p)(_ | _)

  val ws: Parser[String] =
    regex("\\s".r).many.slice

  // Exercise 9.9 - end-of-file
  def eof: Parser[Unit]

  // Exercise 9.9 - root
  def root[A](p: Parser[A]): Parser[A] =
    p *- eof

  // Section 9.5.1
  def label[A](msg: String)(p: Parser[A]): Parser[A]

  // Section 9.5.2
  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  // Section 9.5.3
  def attempt[A](p: Parser[A]): Parser[A]

  // Section 9.5.1
  def errorLocation(e: ParseError): Location

  // Section 9.5.1
  def errorMessage(e: ParseError): String

  // Listing 9.1 - modified without implicit conversions
  implicit class ParserOps[A](pa: Parser[A]) {
    def map[B](f: A => B): Parser[B] = self.map(pa)(f)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(pa)(f)
    def slice = self.slice(pa)
    def many = self.many(pa)
    def within[B, C](pb: Parser[B], pc: Parser[C]): Parser[A] = self.within(pb, pc)(pa)
    def delimited[B](pb: Parser[B]): Parser[List[A]] = self.delimited(pb)(pa)

    def to[B](b: B): Parser[B] = pa.map(_ => b)
    def *-[B](pb: => Parser[B]): Parser[A] = (pa ** pb).map(_._1)
    def -*[B](pb: => Parser[B]): Parser[B] = (pa ** pb).map(_._2)

    // Listing 9.1
    def |[B >: A](pb: => Parser[B]): Parser[B] = self.or(pa, pb)

    // Listing 9.1
    def or[B >: A](pb: => Parser[B]): Parser[B] = self.or(pa, pb)

    // Section 9.2.1
    def **[B](pb: => Parser[B]): Parser[(A, B)] = self.product(pa, pb)

    // Section 9.2.1
    def product[B](pb: => Parser[B]): Parser[(A, B)] = self.product(pa, pb)
  }

  // Listing 9.2
  object Laws {
    import Prop.forAll

    // Listing 9.2
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))

    // Listing 9.2
    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(identity))(in)

    def succeedLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(succeed(42))(s) == Right(42))

    // Exercise 9.2
    def productLaw[A, B](pa: Parser[A], pb: Parser[B])(in: Gen[String]): Prop =
      ??? /* TODO
      forAll(in) { s1 =>
        forAll(in) { s2 =>
          run(pa ** pb)(s1 + s2) == run(pa)(s1) + run(pb)(s2)
        }
      }
      */

    /*
    def labelLaw[A](p: Parser[A], inputs: SGen[String]): Prop =
      forAll(inputs ** Gen.string) {
        case (input, msg) =>
          run(label(msg)(p))(input) match {
            case Left(e) => errorMessage(e) == msg
            case _ => true
          }
      }
    */
  }
}

// Section 9.5.1
case class Location(input: String, offset: Int = 0) {
  private lazy val sliced = input.slice(0, offset + 1)
  lazy val line = sliced.count(_ == '\n') + 1
  lazy val column = sliced.reverse.indexOf('\n')
  /* Section 9.5.1 lists a different implementation:
  lazy val column = sliced.lastIndexOf('\n') match {
    case -1 => offset + 1
    case lineStart => offset - lineStart
  }
  */

  def current: String = input.drop(offset)

  def advanceBy(chars: Int): Location =
    Location(input, offset + chars)

  def toError(message: String): ParseError =
    ParseError(List((this, message)))
}

// Section 9.5.2
case class ParseError(stack: List[(Location, String)]) {

  // Section 9.6.3
  def label[A](s: String): ParseError =
    ParseError(latestLocation.map((_, s)).toList)

  // Section 9.6.3
  def push(location: Location, message: String): ParseError =
    copy(stack = (location, message) :: stack)

  // Section 9.6.3
  def latestLocation: Option[Location] =
    latest.map(_._1)

  // Section 9.6.3
  def latest: Option[(Location, String)] =
    stack.lastOption

  // Exercise 9.16
  override def toString: String =
    stack.reverse match {
      case Nil => ""
      case (location, message) :: context =>
        " " * (location.column - 1) + "^\n" +
          s"line ${location.line}: $message (${context.map(_._2).mkString(" < ")})"
    }
}
