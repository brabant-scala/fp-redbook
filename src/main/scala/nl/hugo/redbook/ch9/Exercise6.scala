package nl.hugo.redbook.ch9

import language.higherKinds

class Exercise6[Parser[+_]](p: Parsers[Parser]) {
  import p._

  // Exercise 9.6
  val parse: Parser[Int] =
    regex("[0-9]".r).map(_.toInt).flatMap(n => listOfN(n, char('a'))).slice.map(_.length)
}