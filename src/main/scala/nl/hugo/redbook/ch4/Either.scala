package nl.hugo.redbook.ch4

// hide std library `Option` and `Either`, since we are writing our own in this chapter
import scala.{ Either => _, Left => _, Option => _, Right => _ }

sealed trait Either[+E, +A] {
  // Exercise 4.06
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(r) => Right(f(r))
    case Left(l) => Left(l)
  }

  // Exercise 4.06
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(r) => f(r)
    case Left(l) => Left(l)
  }

  // Exercise 4.06
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Right(r) => Right(r)
    case Left(_) => b
  }

  // Exercise 4.06
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = this flatMap( aa => b map(bb => f(aa, bb)))
}

case class Left[+E](get: E) extends Either[E, Nothing]

case class Right[+A](get: A) extends Either[Nothing, A]

object Either {
  // Exercise 4.07
  def traverse[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es.foldRight(Right(List.empty): Either[E, List[B]])((oa,ola) => f(oa).map2(ola)(_ :: _))

  // Exercise 4.07
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)(x => x)

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch {
      case e: Exception => Left(e)
    }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch {
      case e: Exception => Left(e)
    }
}
