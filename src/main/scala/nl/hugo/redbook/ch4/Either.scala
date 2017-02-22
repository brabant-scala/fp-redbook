package nl.hugo.redbook.ch4

// hide std library `Option` and `Either`, since we are writing our own in this chapter
import scala.{ Either => _, Left => _, Option => _, Right => _ }

sealed trait Either[+E, +A] {
  // Exercise 4.06
  def map[B](f: A => B): Either[E, B] /*=
    this match {
      case Right(a) => Right(f(a))
      case l: Left[E] => l
    }
    */

  // Exercise 4.06
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] /*=
    map(f) match {
      case Right(b) => b
      case l: Left[E] => l
    }
    */

  // Exercise 4.06
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] /*=
    this match {
      case l: Left[E] => b
      case r => r
    }
    */

  // Exercise 4.06
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] /*=
    flatMap(aa => b.map(bb => f(aa, bb)))
    */
}

case class Left[+E](get: E) extends Either[E, Nothing] {
  override def map[B](f: Nothing => B): Either[E, B] = this
  override def flatMap[EE >: E, B](f: Nothing => Either[EE, B]): Either[EE, B] = this
  override def orElse[EE >: E, B](b: => Either[EE, B]): Either[EE, B] = b
  override def map2[EE >: E, B, C](b: Either[EE, B])(f: (Nothing, B) => C): Either[EE, C] = this
}

case class Right[+A](get: A) extends Either[Nothing, A] {
  override def map[B](f: A => B): Either[Nothing, B] = Right(f(get))
  override def flatMap[EE, B](f: A => Either[EE, B]): Either[EE, B] = f(get)
  override def orElse[EE, B >: A](b: => Either[EE, B]): Either[EE, B] = this
  override def map2[EE, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = b.map(bb => f(get, bb))
}

object Either {
  // Exercise 4.07
  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    traverse_1(as)(f)

  def traverse_1[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as.foldRight[Either[E, List[B]]](Right(Nil))((x, xs) => f(x).map2(xs)(_ :: _))

  def traverse_2[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as match {
      case Nil => Right(Nil)
      case a :: tail => f(a).map2(traverse_2(tail)(f))(_ :: _)
    }

  // Exercise 4.07
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(e => e)

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