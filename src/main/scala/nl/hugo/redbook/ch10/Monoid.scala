package nl.hugo.redbook.ch10

import nl.hugo.redbook.ch7.Nonblocking._
import nl.hugo.redbook.ch7.Nonblocking.Par._ // infix syntax for `Par.map`, `Par.flatMap`, etc
import language.higherKinds

trait Monoid[A] {
  def op(a1: A, a2: A): A

  def zero: A
}

object Monoid {
  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String): String = a1 + a2

    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2

    val zero = Nil
  }

  // Exercise 10.01
  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 + a2

    val zero = 0
  }

  // Exercise 10.01
  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 * a2

    val zero = 1
  }

  // Exercise 10.01
  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 | a2

    val zero = false
  }

  // Exercise 10.01
  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 & a2

    val zero = true
  }

  // Exercise 10.02

  /*
  The option monoid has several possible implementations. For example,
  "right orElse left"  or "left orElse right".For this exercise I choose the latter.
   */
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2

    val zero = None
  }

  // Exercise 10.03

  /*
  Similar to exercise 10.02, there are several possible implementations. Here I
  chose "evaluate the left function and then the right using the result of the
  left function".
   */
  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(a1: A => A, a2: A => A): A => A = a1 andThen a2

    val zero: A => A = a => a
  }

  def flipInputs[A](m: Monoid[A]) = new Monoid[A] {
    def op(a1: A, a2: A): A = m.op(a2, a1)

    val zero: A = m.zero
  }

  def reverseEndoMonoid[A]: Monoid[A => A] = flipInputs(endoMonoid)

  import nl.hugo.redbook.ch8._

  // Exercise 10.04
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
    Prop.forAll(gen) { (a: A) =>
      m.op(m.zero, a) == a &&
        m.op(a, m.zero) == a
    } &&
      Prop.forAll(for {
        a <- gen
        b <- gen
        c <- gen
      } yield (a, b, c)) {
        case (a, b, c) =>
          m.op(a, m.op(b, c)) == m.op(m.op(a, b), c)
      }

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  // Exercise 10.05
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = as
    .foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  // Exercise 10.06

  /*
    We want to iterate over a list, from left to right. We have a function f
    that takes an element from the list and a the current state, and produces a new state.
    by if we curry f, we get f: A => B => B. By applying a, we get a function that transforms
    the current state B to a new state B. So we have to foldMap functions B => B. This is
    something an endMonoid[B] does. The implementation of the endMonoid is "left andThen right'.
    This will take the initial state, and transform it, using the inputs from left to right.
    f.curried(as(0)) andThen f.curried(as(1)) andThen f.curried(as(2) andThen etc etc.
    The endomonoid gives direction to the iteration.
   */
  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, reverseEndoMonoid[B])(f.curried)(z)

  // Exercise 10.06

  /*
    In order to iterate from right to left, we need to change the endoMonoid from a
    left andThen right" to a "right andThen left" order. This can be done with a helper
    function 'flipInputs'. This results in:
    f.curried(as(n-1)) andThen f.curries(as(n-2)) andThen f.curried(as(n-3) AndThen etc etc.
    Again, the endoMonoid gives direction to the iteration
   */

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, endoMonoid[B])(a => b => f(b, a))(z)

  // Exercise 10.07

  /*
    When the array has two or more elements, we split the array. However, when there is one
    element, we transform that element. If there are no elements (when the input is an empty
    sequence, then we just return the zero of the monoid.
   */
  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    as.size match {
      case 0 => m.zero
      case 1 => f(as(0))
      case n =>
        val (l, r) = as.splitAt(n / 2)
        m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
    }

  // Exercise 10.08
  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    def op(a1: Par[A], a2: Par[A]): Par[A] = for {
      l <- a1
      r <- a2
    } yield m.op(l, r)

    val zero: Par[A] = Par.unit(m.zero)
  }

  // Exercise 10.08
  /* Since foldMap is an implementation of 'mapReduce' we can describe the process as two parallel steps; */
  def parMap[A, B](v: IndexedSeq[A], f: A => B): IndexedSeq[Par[B]] = v.map(Par.asyncF(f))

  def parReduce[A](pv: IndexedSeq[Par[A]], m: Monoid[A]): Par[A] = foldMapV(pv, par(m))(x => x)

  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = parReduce(parMap(v, f), m)

  // Exercise 10.09
  def ordered(ints: IndexedSeq[Int]): Boolean = {
    case class RangeState(lower: Int, upper: Int, isOrdered: Boolean) {
      def combine(that: RangeState): RangeState = RangeState(lower, that.upper, isOrdered & that.isOrdered & (upper <= that.lower))
    }

    val orderingMonoid = new Monoid[Option[RangeState]] {
      def op(a1: Option[RangeState], a2: Option[RangeState]): Option[RangeState] = (a1, a2) match {
        case (Some(l), Some(r)) => Some(l.combine(r))
        case (l @ Some(_), _) => l
        case (_, r @ Some(_)) => r
        case _ => None
      }

      val zero = None
    }
    foldMap(ints.toList, orderingMonoid)(i => Some(RangeState(i, i, isOrdered = true))).forall { case RangeState(_, _, b) => b }
  }

  sealed trait WC

  case class Stub(chars: String) extends WC

  case class Part(lStub: String, words: Int, rStub: String) extends WC

  // Exercise 10.10
  // Note: The lazy was added to allow compilation without a concrete implementation.
  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    def op(a1: WC, a2: WC): WC = (a1, a2) match {
      case (Stub(l), Stub(r)) => Stub(l + r)
      case (Part(l, c, r), Stub(s)) => Part(l, c, r + s)
      case (Stub(s), Part(l, c, r)) => Part(s + l, c, r)
      case (Part(ll, lc, lr), Part(rl, rc, rr)) =>
        val joinCount: Int = if ((lr + rl).isEmpty) 0 else 1
        Part(ll, lc + joinCount + rc, rr)
    }

    val zero = Stub("")
  }

  // Exercise 10.11
  def count(s: String): Int = {
    def countStub(s: String): Int = if (s.isEmpty) 0 else 1

    foldMapV(s.toCharArray, wcMonoid)((c: Char) =>
      c match {
        case d if d.isWhitespace => Part("", 0, "")
        case d => Stub(d.toString)
      }) match {
      case Part(l, c, r) => c + countStub(l) + countStub(r)
      case Stub(r) if r.trim.isEmpty => 0
      case _ => 1
    }
  }

  // Exercise 10.16
  def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    def op(l: (A, B), r: (A, B)): (A, B) = (A.op(l._1, r._1), B.op(l._2, r._2))

    val zero: (A, B) = (A.zero, B.zero)
  }

  // Exercise 10.17
  def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    val zero: A => B = _ => B.zero

    def op(a1: A => B, a2: A => B): A => B = a => B.op(a1(a), a2(a))
  }

  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K, V]] {
      val zero: Map[K, V] = Map[K, V]()

      def op(a: Map[K, V], b: Map[K, V]): Map[K, V] =
        (a.keySet ++ b.keySet).foldLeft(zero) { (acc, k) =>
          acc.updated(k, V.op(
            a.getOrElse(k, V.zero),
            b.getOrElse(k, V.zero)
          ))
        }
    }

  // Exercise 10.18
  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    IndexedSeqFoldable.foldMap(as)(v => Map[A, Int](v -> 1))(mapMergeMonoid(intAddition))
}

trait Foldable[F[_]] {

  import Monoid._

  // Exercise 10.12
  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B = foldMap(as)(f.curried)(reverseEndoMonoid[B])(z)

  // Exercise 10.12
  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B = foldMap(as)(a => (b: B) => f(b, a))(endoMonoid[B])(z)

  // Exercise 10.12
  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B = foldLeft(as)(mb.zero)((b, a) => mb.op(b, f(a)))

  // Exercise 10.12
  def concatenate[A](as: F[A])(m: Monoid[A]): A = foldLeft(as)(m.zero)(m.op)

  // Exercise 10.15
  def toList[A](as: F[A]): List[A] = foldRight(as)(List.empty[A])(_ :: _)
}

object ListFoldable extends Foldable[List] {
  // Exercise 10.12
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)

  // Exercise 10.12
  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)

  // Exercise 10.12
  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    as.foldLeft(mb.zero)((b, a) => mb.op(b, f(a)))
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  // Exercise 10.12
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

  // Exercise 10.12
  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

  // Exercise 10.12
  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B = Monoid.foldMapV(as, mb)(f)
}

object StreamFoldable extends Foldable[Stream] {
  // Exercise 10.12
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

  // Exercise 10.12
  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)
}

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  // Exercise 10.13
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = as match {
    case Leaf(a) => f(a)
    case Branch(l, r) => mb.op(foldMap(l)(f)(mb), foldMap(r)(f)(mb))
  }

  // Exercise 10.13
  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = as match {
    case Leaf(a) => f(z, a)
    case Branch(l, r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
  }

  // Exercise 10.13
  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = as match {
    case Leaf(a) => f(a, z)
    case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)
  }
}

object OptionFoldable extends Foldable[Option] {
  // Exerrcise 10.14
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B = as match {
    case None => mb.zero
    case Some(a) => f(a)
  }

  // Exerrcise 10.14
  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B = as match {
    case None => z
    case Some(a) => f(z, a)
  }

  // Exerrcise 10.14
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B = as match {
    case None => z
    case Some(a) => f(a, z)
  }
}
