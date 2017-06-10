package nl.hugo.redbook.ch9

import org.scalacheck.{ Gen, Prop }

import scala.language.{ higherKinds, implicitConversions }

// A parser is a function that analyses a piece of text and tries to extract some information from this. This can be
// Actual information (e.g. an Integer or a string), or structural information (e.g. JSON object, JSON list).. When
// successful, it produces a result, and the number of characters consumed. When there is a failure, it produces a
// parse error. The simplest implementation would be a function:
//   type Parser[A] = String => Either[ParseError, (A, Int)]
// We need the integer to indicate how many characters were consumed from the string.
// However, for efficiency (and location tracking for errors), we can use a Location object, which is a String and an
// offset.
//   type Parser[A] = Location => Either[ParseError, (A, Int)]
// However, we might want to wrap this in specific error types
//   type Parser[A] = Location => Either[Failure, Success[A]]
// Where these type encapsulate that information.

trait Parsers[Parser[+_]] {
  self =>

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  // A method to create a parser that matches the character c.
  implicit def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))

  implicit def string(s: String): Parser[String]

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]

  // Exercise 9.4
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n > 0)
      p.map2(p.listOfN(n - 1))(_ :: _)
    else
      succeed(List.empty)

  // Exercise 9.3
  def many[A](p: Parser[A]): Parser[List[A]] = p.map2(p.many)(_ :: _) or succeed(List.empty)

  def map[A, B](a: Parser[A])(f: A => B): Parser[B]

  // This is the parser's variant of 'unit'
  def succeed[A](a: A): Parser[A] = string("") map (_ => a)

  def slice[A](p: Parser[A]): Parser[String]

  def product[A, B](p: Parser[A], p2: Parser[B]): Parser[(A, B)]

  // Exercise 9.1
  def map2[A, B, C](p: Parser[A], p2: Parser[B])(f: (A, B) => C): Parser[C] = (p ** p2) map f.tupled

  // Exercise 9.1
  def many1[A](p: Parser[A]): Parser[List[A]] = p.map2(p.many)(_ :: _)

  implicit class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def listOfN(n: Int): Parser[List[A]] = self.listOfN(n, p)

    def many: Parser[List[A]] = self.many(p)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def slice: Parser[String] = self.slice(p)

    def many1: Parser[List[A]] = self.many1(p)

    def product[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def **[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def map2[B, C](p2: Parser[B])(f: (A, B) => C): Parser[C] = self.map2(p, p2)(f)
  }

  object Laws {

    val charParserLaw: Prop = Prop.forAll { (c: Char) => run(c)(c.toString) == Right(c) }

    val stringParserLaw: Prop = Prop.forAll { (s: String) => run(s)(s) == Right(s) }

    val orLeftLaw: Prop = Prop.forAll { (sl: String, sr: String) => run(sl or sr)(sl) == Right(sl) }

    val orRightLaw: Prop = Prop.forAll { (sl: String, sr: String) => run(sl or sr)(sr) == Right(sr) }

    val smallInteger: Gen[Int] = Gen.choose(0, 100)

    val listOfNLaw: Prop = Prop.forAll(Gen.alphaNumStr, smallInteger) {
      (v: String, n: Int) =>
        run(v.listOfN(n))(v * n) == Right(v * n)
    }

    val manyLaw: Prop = Prop.forAll(Gen.alphaNumStr, smallInteger) {
      (v: String, n: Int) =>
        run(v.many)(v * n) == Right(List.fill(n)(v))
    }

    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      Prop.forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)

    val succeedLaw: Prop = Prop.forAll((a: String, s: String) => run(succeed(a))(s) == Right(a))

    // Exercise 9.2
    val productLaw: Prop = Prop.forAll { (l: String, r: String) => run(l ** r)(l + r) == Right((l, r)) }

    def flattenL[A, B, C](v: ((A, B), C)): (A, B, C) = (v._1._1, v._1._2, v._2)

    def flattenR[A, B, C](v: (A, (B, C))): (A, B, C) = (v._1, v._2._1, v._2._2)

    // Exersice 9.2
    val productAssociativeLaw: Prop = Prop.forAll { (a: String, b: String, c: String) =>
      run(((a ** b) ** c).map(flattenL))(a + b + c) == run((a ** (b ** c)).map(flattenR))(a + b + c)
    }

    val many1Law: Prop = Prop.forAll(Gen.choose(1, 100), Gen.alphaNumChar) { (n: Int, c: Char) =>
      run(char(c).many1)(c.toString * n) == Right(List.fill(n)(c.toString))
    }
  }

}

case class Location(input: String, offset: Int = 0) {

  lazy val line: Int = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val col: Int = input.slice(0, offset + 1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int): Location = copy(offset = offset + n)

  /* Returns the line corresponding to this location */
  def currentLine: String =
    if (input.length > 1) input.lines.drop(line - 1).next
    else ""
}

case class ParseError(
  stack: List[(Location, String)] = List(),
  otherFailures: List[ParseError] = List()
) {
}