package nl.hugo.redbook.ch3

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
  def tail[A](as: List[A]): List[A] =
    as match {
      case Nil => throw new Exception("Nil does not have a tail")
      case Cons(x, xs) => xs
    }

  // exercise 3.3
  def setHead[A](as: List[A], head: A): List[A] =
    Cons(head, tail(as))

  // exercise 3.4
  @annotation.tailrec
  def drop[A](as: List[A], n: Int): List[A] =
    as match {
      case Nil => Nil
      case _ => if (n == 0) as else drop(tail(as), n - 1)
    }

  // exercise 3.5
  @annotation.tailrec
  def dropWhile[A](as: List[A], f: A => Boolean): List[A] =
    as match {
      case Nil => Nil
      case Cons(x, xs) if f(x) => dropWhile(xs, f)
      case _ => as
    }

  // exercise 3.6
  def init[A](as: List[A]): List[A] =
    init_1(as)

  def init_1[A](as: List[A]): List[A] =
    as match {
      case Nil => throw new Exception("Nil does not have an init")
      case Cons(_, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }

  def init_2[A](as: List[A]): List[A] =
    reverse(as) match {
      case Nil => throw new Exception("Nil does not have an init")
      case Cons(x, xs) => reverse(xs)
    }

  // exercise 3.9
  def length[A](as: List[A]): Int =
    length_1(as)

  def length_1[A](as: List[A]): Int =
    foldLeft(as, 0)((l, x) => l + 1)

  def length_2[A](as: List[A]): Int =
    foldRight(as, 0)((x, l) => l + 1)

  // exercise 3.10
  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  // exercise 3.12
  def reverse[A](as: List[A]): List[A] =
    foldLeft[A, List[A]](as, Nil)((xs, x) => Cons(x, xs))

  // exercise 3.13
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((b, a) => f(a, b))

  // exercise 3.14
  def append[A](l: List[A], r: List[A]): List[A] =
    foldRight[A, List[A]](l, r)(Cons(_, _))

  // exercise 3.18
  def map[A, B](as: List[A])(f: A => B): List[B] =
    foldRight[A, List[B]](as, Nil)((a, bs) => Cons(f(a), bs))

  // exercise 3.19
  def filter[A](as: List[A])(p: A => Boolean): List[A] =
    foldRight[A, List[A]](as, Nil)((x, xs) => if (p(x)) Cons(x, xs) else xs)

  // exercise 3.20
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    reverse(map(as)(f)) match {
      case Cons(x, xs) => foldLeft(xs, x) { (l, r) => append(r, l) }
      case Nil => Nil
    }

  // exercise 3.23
  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = {

    @annotation.tailrec
    def loop(as: List[A], bs: List[B], acc: List[C]): List[C] =
      as match {
        case Nil => acc
        case Cons(i, is) => bs match {
          case Nil => acc
          case Cons(j, js) => loop(is, js, Cons(f(i, j), acc))
        }
      }

    reverse(loop(as, bs, Nil))
  }

  // exercise 3.24
  def contains[A](as: List[A], a: A): Boolean =
    as match {
      case Nil => false
      case Cons(i, is) => (i == a) || contains(is, a)
    }
}