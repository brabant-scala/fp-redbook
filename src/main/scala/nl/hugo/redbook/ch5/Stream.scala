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
  def toList: List[A] = {
    this match {
      case Empty => List()
      case Cons(h, t) => h() :: t().toList
    }
  }

  // Exercise 5.02
  def take(n: Int): Stream[A] = {
    if (n == 0) Empty
    else {
      this match {
        case Empty => Empty
        case Cons(h, t) => cons(h(), t().take(n - 1))
      }
    }
  }

  // Exercise 5.02
  def drop(n: Int): Stream[A] = {
    if (n == 0) this
    else {
      this match {
        case Empty => Empty
        case Cons(h, t) => t().drop(n - 1)
      }
    }
  }

  // Exercise 5.03
  def takeWhile(p: A => Boolean): Stream[A] = {
    this match {
      case Empty => empty
      case Cons(h, t) => {
        val forcedHead = h()
        if (p(forcedHead)) {
          cons(forcedHead, t().takeWhile(p))
        } else empty
      }
    }
  }

  // Exercise 5.04
  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  // Exercise 5.05
  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] = foldRight(empty[A])((a, stream) => if (p(a)) cons(a, stream) else empty)

  // Exercise 5.06
  def headOption: Option[A] = foldRight(Option.empty[A])((a, _) => Option(a))

  // Exercise 5.7
  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((a, stream) => cons(f(a), stream))

  // Exercise 5.7
  def filter(p: A => Boolean): Stream[A] = foldRight(empty[A])((a, stream) => if (p(a)) cons(a, stream) else stream)

  // Exercise 5.7
  def append[B >: A](l: => Stream[B]): Stream[B] = foldRight(l)((a, stream) => cons(a, stream))

  // Exercise 5.7
  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((a, stream) => f(a).append(stream))

  // Exercise 5.13
  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Empty => Option.empty
      case Cons(h, t) => Option(f(h()), t())
    }

  // Exercise 5.13
  def takeViaUnfold(n: Int): Stream[A] =
    unfold((n, this)) {
      case (k, stream) => {
        stream match {
          case Cons(h, t) if k > 0 => Option((h(), (k - 1, t())))
          case _ => Option.empty
        }
      }
    }

  // Exercise 5.13
  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      stream =>
        {
          stream match {
            case Cons(h, t) =>
              val head = h()
              if (p(head)) Option((head, t())) else Option.empty
            case _ => Option.empty
          }
        }
    }

  // Exercise 5.13
  def zipWith[B, C](s: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s)) {
      case (Cons(hA, tA), Cons(hB, tB)) => Option(f(hA(), hB()), (tA(), tB()))
      case _ => Option.empty
    }

  // Exercise 5.13
  def zipAll[B](that: Stream[B]): Stream[(Option[A], Option[B])] = {
    val optStreamA: Stream[Option[A]] = this.map(Option.apply).append(constant(Option.empty[A]))
    val optStreamB: Stream[Option[B]] = that.map(Option.apply).append(constant(Option.empty[B]))

    optStreamA.zipWith(optStreamB)((x, y) => (x, y)).takeWhile(p => p._1.nonEmpty || p._2.nonEmpty)
  }

  // Exercise 5.14
  def startsWith[B](that: Stream[B]): Boolean = {
    val optStreamA: Stream[Option[A]] = this.map(Option.apply).append(constant(Option.empty[A]))
    val optStreamB: Stream[Option[B]] = that.map(Option.apply)

    optStreamA.zipWith(optStreamB) {
      case (Some(a), Some(b)) if (a == b) => true
      case _ => false
    }.forAll(x => x)
  }

  // Exercise 5.15
  def tails: Stream[Stream[A]] = {
    unfold[Stream[A], Option[Stream[A]]](Option(this)) {
      case None => Option.empty
      case Some(Empty) => Option((Empty, Option.empty))
      case Some(Cons(h, t)) => Option((Cons(h, t), Option(t())))
    }
  }

  // Exercise 5.16
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z))) {
      (a, valueAndStream) =>
        {
          val (v, stream) = valueAndStream
          val nextValue = f(a, v)
          (nextValue, cons(nextValue, stream))
        }
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

  // Exercise 5.8
  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  // Exercise 5.9
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  // Exercise 5.10
  def fibs: Stream[Int] = {
    def fibs(k: Int, n: Int): Stream[Int] = cons(k, fibs(n, n + k))
    fibs(0, 1)
  }

  // Exercise 5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z).map[Stream[A]](p => cons(p._1, unfold(p._2)(f))).getOrElse(empty[A])
  }

  // Exercise 5.12
  def fibsViaUnfold: Stream[Int] = unfold[Int, (Int, Int)]((0, 1))(s => Some((s._1, (s._2, s._1 + s._2))))

  // Exercise 5.12
  def fromViaUnfold(n: Int): Stream[Int] = unfold(n)(N => Some((N, N + 1)))

  // Exercise 5.12
  def constantViaUnfold(n: Int): Stream[Int] = unfold(n)(N => Some((N, N)))

  // Exercise 5.12
  def onesViaUnfold: Stream[Int] = constantViaUnfold(1)
}