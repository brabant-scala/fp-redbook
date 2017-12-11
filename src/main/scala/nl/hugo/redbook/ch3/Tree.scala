package nl.hugo.redbook.ch3

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  // exercise 3.29
  def fold[A, B](t: Tree[A])(z: A => B)(f: (B, B) => B): B =
    t match {
      case Leaf(a) => z(a)
      case Branch(l, r) => f(fold(l)(z)(f), fold(r)(z)(f))
    }

  // exercise 3.29
  def size[A](t: Tree[A]): Int =
    fold(t)(_ => 1)(_ + _)

  // exercise 3.29
  def maximum(t: Tree[Int]): Int =
    fold(t)(identity)(_ max _)

  // exercise 3.29
  def depth[A](t: Tree[A]): Int =
    fold(t)(_ => 0)((l, r) => 1 + (l max r))

  // exercise 3.29
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold[A, Tree[B]](t)(a => Leaf(f(a)))(Branch(_, _))
}
