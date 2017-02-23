package nl.hugo.redbook.ch5

import Stream._

trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Cons(h, t) =>
      if (f(h())) Some(h())
      else t().find(f)
    case _ => None
  }

  // Exercise 5.01
  def toList: List[A] =
    this match {
      case Cons(h, t) => h() :: t().toList
      case _ => Nil
    }

  // Exercise 5.02
  def take(n: Int): Stream[A] =
    this match {
      case Cons(h, t) =>
        if (n == 0) Empty
        else Cons(h, () => t().take(n - 1))
      case _ => Empty
    }

  // Exercise 5.02
  def drop(n: Int): Stream[A] =
    drop_1(n)

  def drop_1(n: Int): Stream[A] =
    this match {
      case Cons(_, t) =>
        if (n == 0) this
        else t().drop_1(n - 1)
      case _ => Empty
    }

  def drop_2(n: Int): Stream[A] =
    if (n == 0) this
    else this match {
      case Cons(_, t) => t().drop_2(n - 1)
      case _ => Empty
    }

  // Exercise 5.03
  def takeWhile(p: A => Boolean): Stream[A] =
    takeWhile_1(p)

  def takeWhile_1(p: A => Boolean): Stream[A] =
    this match {
      case Cons(h, t) if p(h()) => Cons(h, () => t().takeWhile_1(p))
      case _ => Empty
    }

  def takeWhile_2(p: A => Boolean): Stream[A] =
    this match {
      case Cons(h, t) =>
        val a = h()
        if (p(a)) cons(a, t().takeWhile_2(p))
        else Empty
      case _ => Empty
    }

  // Exercise 5.04
  def forAll(p: A => Boolean): Boolean =
    this match {
      case Cons(h, t) => p(h()) && t().forAll(p)
      case _ => true
    }

  // Exercise 5.05
  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, as) => if (p(a)) cons(a, as) else Empty)

  // Exercise 5.06
  def headOption: Option[A] =
    this match {
      case Cons(h, t) => Some(h())
      case _ => None
    }

  // Exercise 5.07
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B]) { (a, bs) =>
      cons(f(a), bs)
    }

  // Exercise 5.07
  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A]) { (a, as) =>
      if (p(a)) cons(a, as)
      else as
    }

  // Exercise 5.07
  def append[B >: A](l: => Stream[B]): Stream[B] =
    foldRight(l) { (a, bs) =>
      cons(a, bs)
    }

  // Exercise 5.07
  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B]) { (a, bs) =>
      f(a).append(bs)
    }

  // Exercise 5.13
  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some(f(h()), t())
      case _ => None
    }

  // Exercise 5.13
  def takeViaUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, t), i) if i > 0 => Some(h(), (t(), i - 1))
      case _ => None
    }

  // Exercise 5.13
  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _ => None
    }

  // Exercise 5.13
  def zipWith[B, C](s: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s)) {
      case (Cons(lh, lt), Cons(rh, rt)) =>
        val h = f(lh(), rh())
        val t = (lt(), rt())
        Some(h, t)
      case _ => None
    }

  // Exercise 5.13
  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, s2)) { // how Scala becomes like Lisp! :(
      case (Cons(lh, lt), Cons(rh, rt)) => Some((Some(lh()), Some(rh())), (lt(), rt()))
      case (Cons(h, t), Empty) => Some((Some(h()), None), (t(), Empty))
      case (Empty, Cons(h, t)) => Some((None, Some(h())), (Empty, t()))
      case _ => None
    }

  // Exercise 5.14
  def startsWith[B](s: Stream[B]): Boolean = ???

  // Exercise 5.15
  def tails: Stream[Stream[A]] = ???

  // Exercise 5.16
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = ???
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  // Exercise 5.08
  def constant[A](a: A): Stream[A] =
    cons(a, constant(a))

  // Exercise 5.09
  def from(n: Int): Stream[Int] =
    cons(n, from(n + 1))

  // Exercise 5.10
  def fibs: Stream[Int] = fibs_1

  def fibs_1: Stream[Int] = {
    def go(prev: Int, curr: Int): Stream[Int] =
      cons(prev, go(curr, prev + curr))
    go(0, 1)
  }

  // Exercise 5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None => Empty
    }

  // Exercise 5.12
  def fibsViaUnfold: Stream[Int] =
    unfold((0, 1)) { case (prev: Int, curr: Int) => Some(prev, (curr, prev + curr)) }

  // Exercise 5.12
  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n)((i: Int) => Some(i, i + 1))

  // Exercise 5.12
  def constantViaUnfold(n: Int): Stream[Int] =
    unfold(n)((_: Int) => Some(n, n))

  // Exercise 5.12
  def onesViaUnfold: Stream[Int] =
    constantViaUnfold(1)
}