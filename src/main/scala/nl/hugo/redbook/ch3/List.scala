package nl.hugo.redbook.ch3

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  // initial versio
  //  def sum(ints: List[Int]): Int = ints match {
  //    case Nil => 0
  //    case Cons(x, xs) => x + sum(xs)
  //  }
  //
  //  def product(ds: List[Double]): Double = ds match {
  //    case Nil => 1.0
  //    case Cons(0.0, _) => 0.0
  //    case Cons(x, xs) => x * product(xs)
  //  }
  // improved using foldleft

  def sum(ints: List[Int]): Int = foldLeft(ints, 0)((x, y) => x + y)

  def product(ints: List[Double]): Double = foldLeft(ints, 1.0)((x, y) => x * y)

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // exercise 3.2
  def tail[A](as: List[A]): List[A] = as match {
    case Cons(x, xs) => xs
    case _ => Nil
  }

  // exercise 3.3
  def setHead[A](as: List[A], head: A): List[A] = as match {
    case Cons(x, xs) => Cons(head, tail(as))
    case _ => Nil

  }

  // exercise 3.4
  def drop[A](as: List[A], n: Int): List[A] = n match {
    case n if n > 0 => drop(tail(as), n - 1)
    case _ => as
  }

  // exercise 3.5
  def dropWhile[A](as: List[A], f: A => Boolean): List[A] = as match {
    case Cons(b, bs) if f(b) => dropWhile(bs, f)
    case _ => as
  }

  // exercise 3.6
  def init[A](as: List[A]): List[A] = as match {
    case Cons(b, Nil) => Nil
    case Cons(b, bs) => Cons(b, init(bs))
    case _ => Nil
  }

  // exercise 3.9
  def length[A](as: List[A]): Int = foldRight(as, 0)((x, y) => y + 0)

  // exercise 3.10
  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(b, bs) => foldLeft(bs, f(z, b))(f)
  }


  // exercise 3.12
  def reverse[A](as: List[A]): List[A] = {

    foldLeft(as, List[A]())((b, a) => Cons(a, b))
  }

  //  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match{

  // exercise 3.13
  //  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = ???
  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Cons(b, bs) => f(b, foldRight(bs, z)(f))
    case Nil => z
  }

  //    foldLeft(reverse(l), z)((b,a) => f(a,b))

  // exercise 3.14
  def append[A](l: List[A], r: List[A]): List[A] = foldRight(l, r)((b, a) => Cons(b, a))

  // exercise 3.18
  def map[A, B](as: List[A])(f: A => B): List[B] = as match {
    case Cons(b, bs) => Cons(f(b), map(bs)(f))
    case Nil => Nil
  }

  def filter[A](as: List[A])(p: A => Boolean): List[A] = as match{
    case Cons(b,bs) if(p(b)) => Cons(b, filter(bs)(p))
    case Cons(b,bs) => filter(bs)(p)
    case Nil => Nil
  }

  // exercise 3.20
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = as match{
    case Cons(b,bs) => append(f(b), flatMap(bs)(f))
    case Nil => List[B]()
  }

  def add(l: List[Int], r: List[Int]): List[Int] = ???

  // exercise 3.23
  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = ???

  // exercise 3.24
  def contains[A](as: List[A], a: A): Boolean = ???
}