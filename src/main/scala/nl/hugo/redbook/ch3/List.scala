package nl.hugo.redbook.ch3

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // exercise 3.2
  def tail[A](as: List[A]): List[A] = as match {
    case Nil => throw new Exception("Cannot determine the tail of an empty list")
      // Alternatively we could choose to return Nil for the tail of an empty list
    case Cons(_, xs) => xs
  }

  // exercise 3.3
  def setHead[A](as: List[A], head: A): List[A] = as match {
    case Nil => throw new Exception("Cannot replace the head of an empty list")
    case Cons(_, xs) => Cons(head, xs)
  }

  // exercise 3.4
  def drop[A](as: List[A], n: Int): List[A] = as match {
//    case Nil if n > 0 => throw new Exception("Cannot drop more elements than present in the list")
    case Cons(_, xs) if n > 0 => drop(xs, n-1)
    case other => other
  }

  // exercise 3.5
  def dropWhile[A](as: List[A], f: A => Boolean): List[A] = as match {
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case other => other
  }

  // exercise 3.6
  def init[A](as: List[A]): List[A] = as match {
    case Nil => throw new Exception("Cannot take all except last element from an empty list")
    case Cons(_, Nil) => Nil
    case Cons(x, xs)  => Cons(x, init(xs))
  }

  // exercise 3.9
  def length[A](as: List[A]): Int = foldRight(as, 0)((_,c) => c + 1)

  // exercise 3.10
  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil         => z
    case Cons(x, xs) => foldLeft(xs, f(z,x))(f)
  }

  // exercise 3.12
  def reverse[A](as: List[A]): List[A] = foldLeft(as, Nil: List[A])((result, current) => Cons(current, result))

  // exercise 3.13
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil         => z
    case Cons(x, xs) => f(x, foldRight(xs,z)(f))
  }

  // exercise 3.14
  def append[A](l: List[A], r: List[A]): List[A] = foldRight(l, r)((current, result) => Cons(current, result))
  def appendL[A](l: List[A], r: List[A]): List[A] = foldLeft(reverse(l), r)((result, current) => Cons(current, result))

  // exercise 3.18
  def map1[A, B](as: List[A])(f: A => B): List[B] = as match {
    case Nil         => Nil
    case Cons(x, xs) => Cons(f(x), map(xs)(f))
  }
  // TODO Can also write this as foldRight
  def map[A, B](as: List[A])(f: A => B): List[B] = List.foldRight(as, Nil: List[B])((i,a) => Cons(f(i), a))

  // exercise 3.19
  def filter1[A](as: List[A])(p: A => Boolean): List[A] = as match {
    case Nil                 => Nil
    case Cons(x, xs) if p(x) => Cons(x, filter(xs)(p))
    case Cons(_, xs)         => filter(xs)(p)
  }
  // TODO Can also write this as foldRight
  def filter[A](as: List[A])(p: A => Boolean): List[A] = List.foldRight(as, Nil: List[A])((i,a) => if (p(i)) Cons(i, a) else a)

  // exercise 3.20
  def flatMap1[A, B](as: List[A])(f: A => List[B]): List[B] = as match {
    case Nil         => Nil
    case Cons(x, xs) => append(f(x), flatMap(xs)(f))
  }
  // TODO Can also use flatAppend and map
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = List.foldRight(as, Nil: List[B])((i,a) => List.append(f(i), a))

  // exercise 3.23
  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = (as, bs) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(ah,at), Cons(bh,bt)) => Cons(f(ah,bh), zipWith(at, bt)(f))
  }

  // exercise 3.24
  def contains[A](as: List[A], a: A): Boolean = ???
}
