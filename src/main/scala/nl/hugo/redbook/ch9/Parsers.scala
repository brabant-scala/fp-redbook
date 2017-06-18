package nl.hugo.redbook.ch9

import java.util.regex.Pattern

import org.scalacheck.{Gen, Prop}

import scala.language.{higherKinds, implicitConversions}
import scala.util.matching.Regex

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

trait Parsers[Parser[+ _]] {
  self =>

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  // A method to create a parser that matches the character c.
  implicit def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))

  implicit def string(s: String): Parser[String]

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  implicit def regex(r: Regex): Parser[String]

  // Exercise 9.6
  def toNumber: Parser[Int] =
    for {
      v <- "[0-9]+".r
      n = v.toInt
      l = listOfN(n, "a")
    } yield v.toInt

  // Exercise 9.5
  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  // Exercise 9.4
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n > 0)
      p.map2(p.listOfN(n - 1))(_ :: _)
    else
      succeed(List.empty)

  // Exercise 9.3
  def many[A](p: Parser[A]): Parser[List[A]] = p.map2(p.many)(_ :: _) or succeed(List.empty)

  // Exercise 9.8
  def map[A, B](a: Parser[A])(f: A => B): Parser[B] = a.flatMap(b => succeed(f(b)))

  // This is the parser's variant of 'unit'
  def succeed[A](a: A): Parser[A] = string("") map (_ => a)

  def slice[A](p: Parser[A]): Parser[String]

  //Exercise 9.6
  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    for {
      a <- p
      b <- p2
    } yield f(a, b)

  //Exercise 9.6
  def product[A, B](p: Parser[A], p2: Parser[B]): Parser[(A, B)] =
    for {
      a <- p
      b <- p2
    } yield (a, b)

  // Exercise 9.1 & 9.5
  def map2_9_1[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] = (p ** p2) map f.tupled

  // Exercise 9.1
  def many1[A](p: Parser[A]): Parser[List[A]] = p.map2(p.many)(_ :: _)

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  // Exercise 9.9
  def whiteSpaces: Parser[List[String]] = (" " | "\n" | "\r" | "\t").many

  // Exercise 9.9
  def dropLeft[A, B](l: Parser[A], r: Parser[B]): Parser[B] =
    for {
      _ <- l
      v <- r
    } yield v

  // Exercise 9.9
  def dropRight[A, B](l: Parser[A], r: Parser[B]): Parser[A] =
    for {
      v <- l
      _ <- r
    } yield v

  // Exercise 9.9
  def surround[A, B, C](p: Parser[A], l: Parser[B], r: Parser[C]): Parser[A] =
    l <| p |> r

  // Exercise 9.9
  def eof: Parser[String] =
    regex("\\z".r)

  // Exercise 9.9
  def root[A](p: Parser[A]): Parser[A] = p |> eof

  // Exercise 9.9
  def sep1[A, B](p: Parser[A], s: Parser[B]): Parser[List[A]] =
    for {
      f <- p
      r <- (s <| p).many
    } yield f :: r

  // Exercise 9.9
  def sep[A, B](p: Parser[A], s: Parser[B]): Parser[List[A]] =
    p sep1 s or succeed(List.empty)

  // Exercise 9.9
  def thru(s: String): Parser[String] = (".*?" + Pattern.quote(s)).r

  // Exercise 9.9
  /** Unescaped string literals, like "foo" or "bar". */
  def quoted: Parser[String] = string("\"") <| thru("\"").map(_.dropRight(1))

  // Exercise 9.9
  /** Floating point literals, converted to a `Double`. */
  def double: Parser[Double] =
    doubleString map (_.toDouble)

  // Exercise 9.9
  def doubleString: Parser[String] =
    token("[-+]?([0-9]*\\.)?[0-9]+([eE][-+]?[0-9]+)?".r)

  // Exercise 9.9
  def token[A](p: Parser[A]): Parser[A] =
    p |> whiteSpaces

  implicit class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def listOfN(n: Int): Parser[List[A]] = self.listOfN(n, p)

    def many: Parser[List[A]] = self.many(p)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def slice: Parser[String] = self.slice(p)

    def many1: Parser[List[A]] = self.many1(p)

    def product[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def **[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def map2[B, C](p2: => Parser[B])(f: (A, B) => C): Parser[C] = self.map2(p, p2)(f)

    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)

    def <|[B](r: Parser[B]): Parser[B] = self.dropLeft(p, r)

    def |>[B](r: Parser[B]): Parser[A] = self.dropRight(p, r)

    def surround[B, C](l: Parser[B], r: Parser[C]): Parser[A] = self.surround(p, l, r)

    def as[B](b: B): Parser[B] = self.map(self.slice(p))(_ => b)

    def sep[B](s: Parser[B]): Parser[List[A]] = self.sep(p, s)

    def sep1[B](s: Parser[B]): Parser[List[A]] = self.sep1(p, s)
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

trait JSON

object JSON {

  case object JNull extends JSON

  case class JNumber(get: Double) extends JSON

  case class JString(get: String) extends JSON

  case class JBool(get: Boolean) extends JSON

  case class JArray(get: IndexedSeq[JSON]) extends JSON

  case class JObject(get: Map[String, JSON]) extends JSON

  def jsonParser[Err, Parser[+ _]](P: Parsers[Parser]): Parser[JSON] = {
    import P._

    // A value can be a string in double quotes, or a number, or true or false or null, or an object or an array.
    // These structures can be nested.
    def value: Parser[JSON] = {
      "null".as(JNull) |
        double.map(JNumber) |
        quoted.map(JString) |
        "true".as(JBool(true)) |
        "false".as(JBool(false)) |
        obj |
        array
    }

    // An object is an unordered set of name/value pairs. An object begins with { (left brace) and ends with }
    // (right brace). Each name is followed by : (colon) and the name/value pairs are separated by , (comma).
    def obj: Parser[JObject] = {
      ((quoted ** (":" <| value)) sep "," map (kvs => JObject(kvs.toMap)))
        .surround("{", "}")
    }

    // An array is an ordered collection of values. An array begins with [ (left bracket) and ends with ]
    // (right bracket). Values are separated by , (comma).
    def array: Parser[JArray] = {
      (value sep "," map (vs => JArray(vs.toIndexedSeq)))
        .surround("[", "]")
    }
    // JSON is built on two structures:
    //  * A collection of name/value pairs. In various languages, this is realized as an object, record, struct,
    //    dictionary, hash table, keyed list, or associative array.
    //  * An ordered list of values. In most languages, this is realized as an array, vector, list, or sequence.
    root(obj | array)
  }
}