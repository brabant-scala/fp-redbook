package nl.hugo.redbook.ch4

import scala.{ Either => _, Option => _, Some => _ }

// hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  // Exercise 4.01
  def map[B](f: A => B): Option[B] /* =
    this match {
      case Some(a) => Some(f(a))
      case None => None
    }
    */

  // Exercise 4.01
  def getOrElse[B >: A](default: => B): B /* =
    this match {
      case Some(a) => a
      case None => default
    }
    */

  // Exercise 4.01
  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f).getOrElse(None)

  // Exercise 4.01
  def orElse[B >: A](ob: => Option[B]): Option[B] =
    map(Some(_)).getOrElse(ob)

  // Exercise 4.01
  def filter(f: A => Boolean): Option[A] =
    if (map(f).getOrElse(false)) this else None
}

case class Some[+A](get: A) extends Option[A] {
  override def map[B](f: A => B): Option[B] = Some(f(get))
  override def getOrElse[B >: A](default: => B): B = get
}

case object None extends Option[Nothing] {
  override def map[B](f: Nothing => B): Option[B] = None
  override def getOrElse[B >: Nothing](default: => B): B = default
}

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    } catch {
      case e: Exception => 43
    } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    } catch {
      case e: Exception => 43
    }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  // Exercise 4.02
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap { m =>
      mean(xs.map(x => math.pow(x - m, 2)))
    }

  // Exercise 4.03
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    map2_1(a, b)(f)

  def map2_1[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(aa => b.map(bb => f(aa, bb)))

  def map2_2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)

  def map2_3[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    (a, b) match {
      case (Some(aa), Some(bb)) => Some(f(aa, bb))
      case _ => None
    }

  // Exercise 4.04
  def sequence[A](as: List[Option[A]]): Option[List[A]] =
    sequence_1(as)

  def sequence_1[A](as: List[Option[A]]): Option[List[A]] =
    as.foldRight[Option[List[A]]](Some(Nil))((x, xs) => map2(x, xs)(_ :: _))

  def sequence_2[A](as: List[Option[A]]): Option[List[A]] =
    as match {
      case Nil => Some(Nil)
      case a :: tail => map2(a, sequence(tail))(_ :: _)
    }

  // Exercise 4.05
  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    traverse_1(as)(f)

  def traverse_1[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as.foldRight[Option[List[B]]](Some(Nil))((x, xs) => map2(f(x), xs)(_ :: _))

  def traverse_2[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as match {
      case Nil => Some(Nil)
      case a :: tail => map2(f(a), traverse(tail)(f))(_ :: _)
    }

  // Exercise 4.05
  def sequence_via_traverse[A](as: List[Option[A]]): Option[List[A]] =
    traverse(as)(a => a)
}