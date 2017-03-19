package nl.hugo.redbook.ch5

import Stream._

trait Stream[+A] {

  // NOTE: improved by making both arguments to f lazy
  def foldRight[B](z: => B)(f: (=> A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def headOption: Option[A] =
    this match {
      case Cons(h, t) => Some(h())
      case _ => None
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
    toList_1

  def toList_1: List[A] =
    this match {
      case Cons(h, t) =>
        h() :: t().toList
      case _ =>
        Nil
    }

  def toList_2: List[A] =
    foldRight(List.empty[A]) { (a, as) =>
      a :: as
    }

  // Exercise 5.02
  def take(n: Int): Stream[A] =
    this match {
      case Cons(h, t) if n != 0 =>
        Cons(h, () => t().take(n - 1))
      case _ =>
        Empty
    }

  // Exercise 5.02
  def drop(n: Int): Stream[A] =
    drop_1(n)

  def drop_1(n: Int): Stream[A] =
    this match {
      case Cons(_, t) if n != 0 =>
        t().drop_1(n - 1)
      case _ =>
        this
    }

  def drop_2(n: Int): Stream[A] =
    if (n == 0) this
    else this match {
      case Cons(_, t) =>
        t().drop_2(n - 1)
      case _ =>
        Empty
    }

  // Exercise 5.03
  def takeWhile(p: A => Boolean): Stream[A] =
    takeWhile_2(p)

  // takeWhile - shorter but less optimal
  def takeWhile_1(p: A => Boolean): Stream[A] =
    this match {
      case Cons(h, t) if p(h()) =>
        Cons(h, () => t().takeWhile_1(p)) // does not memoize h() after evaluation
      case _ =>
        Empty
    }

  // takeWhile - optimal
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
    forAll_1(p)

  // forAll - using foldRight
  def forAll_1(p: A => Boolean): Boolean =
    foldRight(true) { (a, b) =>
      p(a) && b // order is important for short-circuit
    }

  // forAll - using match
  def forAll_2(p: A => Boolean): Boolean =
    this match {
      case Cons(h, t) =>
        p(h()) && t().forAll(p)
      case _ =>
        true
    }

  // Exercise 5.05
  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(empty[A]) { (a, as) =>
      if (p(a)) cons(a, as)
      else Empty
    }

  // Exercise 5.06
  def headOptionBiaFoldRight: Option[A] =
    foldRight[Option[A]](None) { (a, _) =>
      Some(a)
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
  def append[B >: A](r: => Stream[B]): Stream[B] =
    foldRight(r) { (a, bs) =>
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
      case Cons(h, t) =>
        Some(f(h()), t())
      case _ =>
        None
    }

  // Exercise 5.13
  def takeViaUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, t), i) if i > 0 =>
        Some(h(), (t(), i - 1))
      case _ =>
        None
    }

  // Exercise 5.13
  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if p(h()) => // TODO: tests show that h() is not evaluated twice; why?
        Some(h(), t())
      case _ =>
        None
    }

  // Exercise 5.13
  def zipWith[B, C](s: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s)) {
      case (Cons(ah, at), Cons(bh, bt)) =>
        val h = f(ah(), bh())
        val t = (at(), bt())
        Some(h, t)
      case _ =>
        None
    }

  // Exercise 5.13
  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    zipAll_1(s2)

  // zipAll - based on unfold
  def zipAll_1[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, s2)) { // how Scala becomes like Lisp! :(
      case (Cons(ah, at), Cons(bh, bt)) => Some((Some(ah()), Some(bh())), (at(), bt()))
      case (Cons(ah, at), Empty) => Some((Some(ah()), None), (at(), Empty))
      case (Empty, Cons(bh, bt)) => Some((None, Some(bh())), (Empty, bt()))
      case _ => None
    }

  // zipAll - based on zipWith
  def zipAll_2[B](bs: Stream[B]): Stream[(Option[A], Option[B])] = {
    val s1 = map(Some(_)).append(constant(None))
    val s2 = bs.map(Some(_)).append(constant(None))
    s1.zipWith(s2)((a, b) => (a, b)).takeWhile {
      case (a, b) => a.nonEmpty || b.nonEmpty
    }
  }

  // Exercise 5.14
  def startsWith[B](s: Stream[B]): Boolean =
    startsWith_2(s)

  // startsWith - based on zipAll
  def startsWith_1[A](s: Stream[A]): Boolean =
    zipAll(s).takeWhile(_._2 != None).forAll {
      case (l, r) => l == r
    }

  // startsWith - more lazy version
  def startsWith_2[A](s: Stream[A]): Boolean =
    (this, s) match {
      case (Cons(lh, lt), Cons(rh, rt)) if lh() == rh() =>
        lt().startsWith(rt())
      case (_, Empty) =>
        true
      case _ =>
        false
    }

  // Exercise 5.15
  def tails: Stream[Stream[A]] =
    tails_1

  // tails - concise and efficient
  def tails_1: Stream[Stream[A]] = {
    val result = unfold(this) {
      case c @ Cons(_, t) =>
        Some(c, t())
      case _ =>
        None
    }
    result.append(Stream(Empty))
  }

  // tails - alternative using drop
  def tails_2: Stream[Stream[A]] = {
    val result = unfold(this) {
      case Empty =>
        None
      case s =>
        Some(s, s.drop(1))
    }
    result.append(Stream(Empty))
  }

  // Exercise 5.16 - return type could be Cons[B]
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    scanRight_1(z)(f)

  // scanRight - based on foldRight
  def scanRight_1[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight(Cons(() => z, () => Empty)) { (a, prev) =>
      Cons(() => f(a, prev.h()), () => prev)
    }

  // scanRight - by hand
  def scanRight_2[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight(z, Stream(z)) { (a, bs) =>
      lazy val prev = bs // make bs lazy to ensure 1 eval
      val next = f(a, prev._1)
      (next, cons(next, prev._2))
    }._2
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
  val fibs: Stream[Int] = {
    def go(prev: Int, curr: Int): Stream[Int] =
      cons(prev, go(curr, prev + curr))
    go(0, 1)
  }

  // Exercise 5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((a, s)) =>
        cons(a, unfold(s)(f))
      case None =>
        Empty
    }

  // Exercise 5.12
  def fibsViaUnfold: Stream[Int] =
    unfold((0, 1)) {
      case (prev: Int, curr: Int) =>
        Some(prev, (curr, prev + curr))
    }

  // Exercise 5.12
  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n) { (i: Int) =>
      Some(i, i + 1)
    }

  // Exercise 5.12
  def constantViaUnfold[A](a: A): Stream[A] =
    unfold(a) { (_: A) =>
      Some(a, a)
    }

  // Exercise 5.12
  def onesViaUnfold: Stream[Int] =
    constantViaUnfold(1)
}