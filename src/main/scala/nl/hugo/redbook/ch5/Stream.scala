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
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  // Exercise 5.01
  def toList: List[A] = foldRight(List.empty[A])((h,t) => h +: t)

  // Exercise 5.02
  def take(n: Int): Stream[A] = this match {
    case Cons(h,t) if n > 0 => cons(h(), t().take(n-1))
    case _ => empty
  }

  // Exercise 5.02
//  def drop(n: Int): Stream[A] = this match {
//    case Cons(h,t) if n > 0 => t().drop(n - 1)
//    case Cons(h,t) => this
//    case Empty => empty
//  }
  def drop(n: Int): Stream[A] = this match {
    case Cons(h,t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  // Exercise 5.03
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h,t) if p(h()) => cons(h(), t().takeWhile(p)) // Evalueert nu wel 2x functie h(), zou je "moeten" cachen in een val voor snelheid
    case _ => empty
  }

  // Exercise 5.04
//  def forAll(p: A => Boolean): Boolean = foldRight(true)((a,r) => r && p(a)) // is niet goed. moet hem omdraaien
  def forAll(p: A => Boolean): Boolean = foldRight(true)((a,r) => p(a) && r)  // let op : r is het passed by name dus moet rechts

  // Exercise 5.05
  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
//    foldRight((true, empty): (Boolean, Stream[A]))((a,r) =>
//      (r._1 && p(a), if (r._1 && p(a)) cons(a, r._2) else r._2)
//    )._2
    foldRight(empty:Stream[A])((a,b)=> if (p(a)) cons(a,b) else empty)

  // Exercise 5.06
  def headOption: Option[A] = this match {
    case Cons(h,t) => Option(h())
    case _ => None
  }

  // Exercise 5.7
  def map[B](f: A => B): Stream[B] = this match {
    case Cons(h,t) => cons(f(h()), t().map(f))
    case Empty => empty
  }
// probeer deze opgaves (5.7) ook eens met foldRight

  // Exercise 5.7
  def filter(p: A => Boolean): Stream[A] = this match {
    case Cons(h,t) if p(h()) => cons(h(), t().filter(p))
    case Cons(_,t) => t().filter(p)
    case Empty => empty
  }

  // Exercise 5.7
  def append[B >: A](l: => Stream[B]): Stream[B] = this match {
    case Cons(h,t) => cons(h(), t().append(l))
    case Empty => l
  }

  // Exercise 5.7
  def flatMap[B](f: A => Stream[B]): Stream[B] = this match {
    case Cons(h,t) => f(h()).append(t().flatMap(f))
    case Empty => empty
  }

  // Exercise 5.13
  def mapViaUnfold[B](f: A => B): Stream[B] = ???

  // Exercise 5.13
  def takeViaUnfold(n: Int): Stream[A] = ???

  // Exercise 5.13
  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = ???

  // Exercise 5.13
  def zipWith[B, C](s: Stream[B])(f: (A, B) => C): Stream[C] = ???

  // Exercise 5.13
  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = ???

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

  // Exercise 5.8
  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  // Exercise 5.9
  def from(n: Int): Stream[Int] = cons(n, from(n+1))

  // Exercise 5.10
  def fibs: Stream[Int] = {
    def next(p: Int, c: Int): Stream[Int] = cons(p+c, next(c, p+c))
    cons(0, cons(1, next(0, 1)))
  }
  def fibs2: Stream[Int] = {
    def next(p:Int, c:Int): Stream[Int] = cons(p+c, next(p, p+c))
    cons(0, next(0, 1))

//    0
//    1
//    0 + 1 = 1    p==0 + c==1
//    1 + 1 = 2    p==1 + c==1
//    1 + 2 = 3    p==1 + c==2
//    2 + 3 = 5    p==2 + c==3
//    3 + 5 = 8    p==3 + c==5
  }

  // Hugo heeft testen aangepast, zodat ook de efficientie gemonitored wordt.

  // Exercise 5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((a,s)) => cons(a, unfold(s)(f))
      case _ => Empty
    }
  }

  // Exercise 5.12
  def fibsViaUnfold: Stream[Int] = ???

  // Exercise 5.12
  def fromViaUnfold(n: Int): Stream[Int] = ???

  // Exercise 5.12
  def constantViaUnfold[A](a: A): Stream[A] = ???

  // Exercise 5.12
  def onesViaUnfold: Stream[Int] = ???
}
