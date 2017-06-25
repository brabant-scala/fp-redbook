package nl.hugo.redbook.ch9

import scala.util.Try

object Exercise12 {

  case class Error(location: Location, message: String) {
    override def toString: String = s"$location: $message"
  }

  type Parser1[+A] = String => Either[Error, A]

  type Parser2[+A] = String => Try[A]
  case class ParseException(error: Error) extends Exception(error.toString)

  type Parser3[+A] = String => Result[A]
  sealed trait Result[+A]
  case class Success[+A](value: A) extends Result[A]
  case class Failure(error: Error) extends Result[Nothing]
}