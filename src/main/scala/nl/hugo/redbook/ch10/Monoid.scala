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
    def op(i1: Int, i2: Int): Int = i1 + i2
    def zero: Int = 0
  }

  // Exercise 10.01
  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(i1: Int, i2: Int): Int = i1 * i2
    def zero: Int = 1
  }

  // Exercise 10.01
  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(b1: Boolean, b2: Boolean): Boolean = b1 || b2
    def zero: Boolean = false
  }

  // Exercise 10.01
  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(b1: Boolean, b2: Boolean): Boolean = b1 && b2
    def zero: Boolean = true
  }

  // Exercise 10.02
  def optionMonoid[A]: Monoid[Option[A]] = optionMonoid_1

  private def optionMonoid_1[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(o1: Option[A], o2: Option[A]): Option[A] = o1 orElse o2
    def zero: Option[A] = None
  }

  private def optionMonoid_2[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(o1: Option[A], o2: Option[A]): Option[A] = o2 orElse o1
    def zero: Option[A] = None
  }

  private def optionMonoid_3[A]: Monoid[Option[A]] = swap(optionMonoid_1)

  def swap[A](ma: Monoid[A]): Monoid[A] = new Monoid[A] {
    val zero: A = ma.zero
    def op(l: A, r: A): A = ma.op(r, l)
  }

  // Exercise 10.03
  def endoMonoid[A]: Monoid[A => A] = endoMonoid_1

  private def endoMonoid_1[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(f1: A => A, f2: A => A): A => A = f1 compose f2
    def zero: A => A = identity
  }

  private def endoMonoid_2[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(f1: A => A, f2: A => A): A => A = f2 compose f1
    def zero: A => A = identity
  }

  private def endoMonoid_3[A]: Monoid[A => A] = swap(endoMonoid_1)

  import nl.hugo.redbook.ch8._

  // Exercise 10.04
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
    Prop.forAll(gen) {
      a => (m.op(a, m.zero) == a) && (m.op(m.zero, a) == a)
    } &&
      Prop.forAll((gen ** gen) ** gen) {
        case ((a, b), c) => m.op(a, m.op(b, c)) == m.op(m.op(a, b), c)
      }

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  // Exercise 10.05
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = foldMap_1(as, m)(f)

  private def foldMap_1[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    concatenate(as.map(f), m)

  private def foldMap_2[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  // Exercise 10.06
  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(f.curried)(z)

  // Exercise 10.06
  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, swap(endoMonoid[B]))(a => b => f(b, a))(z)

  // Exercise 10.07
  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    as.size match {
      case 0 => m.zero
      case 1 => f(as(0))
      case _ =>
        val (l, r) = as.splitAt(as.size / 2)
        m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
    }

  // Exercise 10.08
  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    def op(p1: Par[A], p2: Par[A]): Par[A] = Par.map2(p1, p2)(m.op)
    def zero: Par[A] = Par.unit(m.zero)
  }

  // Exercise 10.08
  def parFoldMap[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = parFoldMap_2(as, m)(f)

  // only calling f in parallel
  def parFoldMap_1[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    foldMapV(as.toIndexedSeq, par(m))(Par.asyncF(f))

  // folding in parallel as well
  def parFoldMap_2[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = {
    val pbs = as.map(Par.asyncF(f))
    sequence(pbs).flatMap { bs =>
      foldMapV(bs, par(m))(b => Par.lazyUnit(b))
    }
  }

  // Exercise 10.09
  def ordered[A: Ordering](as: IndexedSeq[A]): Boolean = {
    import scala.math.Ordering.Implicits._

    case class State(ordered: Boolean, min: A, max: A)

    val monoid = new Monoid[Option[State]] {
      val zero = None
      def op(s1: Option[State], s2: Option[State]) =
        (s1, s2) match {
          case (Some(State(o1, min1, max1)), Some(State(o2, min2, max2))) =>
            Some(State(o1 && o2 && (max1 <= min2), min1 min min2, max1 max max2))
          case (s, None) => s
          case (None, s) => s
        }
    }

    foldMapV(as, monoid)(a => Some(State(true, a, a))).map(_.ordered).getOrElse(true)
  }

  sealed trait WC

  case class Stub(chars: String) extends WC

  case class Part(lStub: String, words: Int, rStub: String) extends WC

  // Exercise 10.10
  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    val zero: WC = Stub("")
    def op(l: WC, r: WC): WC =
      (l, r) match {
        case (Part(l1, w1, r1), Part(l2, w2, r2)) if (r1 + l2).nonEmpty =>
          Part(l1, w1 + w2 + 1, r2)
        case (Part(l1, w1, r1), Part(l2, w2, r2)) =>
          Part(l1, w1 + w2, r2)
        case (Part(l1, w1, r1), Stub(s2)) =>
          Part(l1, w1, r1 + s2)
        case (Stub(s1), Part(l2, w2, r2)) =>
          Part(s1 + l2, w2, r2)
        case (Stub(s1), Stub(s2)) =>
          Stub(s1 + s2)
      }
  }

  // Exercise 10.11
  def count(s: String): Int = {

    def wc(c: Char): WC =
      if (c.isWhitespace)
        Part("", 0, "")
      else
        Stub(c.toString)

    def cnt(s: String): Int =
      if (s.nonEmpty) 1 else 0

    Monoid.foldMapV(s.toIndexedSeq, wcMonoid)(wc) match {
      case Stub(s) => cnt(s)
      case Part(l, w, r) => cnt(l) + w + cnt(r)
    }
  }

  // Exercise 10.16
  def productMonoid[A, B](ma: Monoid[A], mb: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    val zero: (A, B) =
      (ma.zero, mb.zero)
    def op(l: (A, B), r: (A, B)): (A, B) =
      (ma.op(l._1, r._1), mb.op(l._2, r._2))
  }

  // Exercise 10.17
  def functionMonoid[A, B](mb: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    val zero: A => B =
      _ => mb.zero
    def op(l: A => B, r: A => B): A => B =
      a => mb.op(l(a), r(a))
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
    foldMapV(as, mapMergeMonoid[A, Int](intAddition))(a => Map(a -> 1))
}

trait Foldable[F[_]] {

  import Monoid._

  // Exercise 10.12
  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B

  // Exercise 10.12
  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B

  // Exercise 10.12
  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero)((b, a) => mb.op(b, f(a)))

  // Exercise 10.12
  def concatenate[A](as: F[A])(ma: Monoid[A]): A =
    foldLeft(as)(ma.zero)(ma.op)

  // Exercise 10.15
  def toList[A](as: F[A]): List[A] =
    foldRight(as)(List.empty[A])(_ :: _)
}

object ListFoldable extends Foldable[List] {
  // Exercise 10.12
  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)

  // Exercise 10.12
  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)

  override def toList[A](as: List[A]): List[A] =
    as
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  // Exercise 10.12
  def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)

  // Exercise 10.12
  def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)
}

object StreamFoldable extends Foldable[Stream] {
  // Exercise 10.12
  def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)

  // Exercise 10.12
  def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)
}

import nl.hugo.redbook.ch3.{ Branch, Leaf, Tree }

object TreeFoldable extends Foldable[Tree] {
  // Exercise 10.13
  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B =
    as match {
      case Leaf(a) =>
        f(a, z)
      case Branch(l, r) =>
        val rb = foldRight(r)(z)(f)
        foldRight(l)(rb)(f)
    }

  // Exercise 10.13
  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B =
    as match {
      case Leaf(a) =>
        f(z, a)
      case Branch(l, r) =>
        val lb = foldLeft(l)(z)(f)
        foldLeft(r)(lb)(f)
    }

  // Exercise 10.13
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
    as match {
      case Leaf(a) =>
        f(a)
      case Branch(l, r) =>
        val lb = foldMap(l)(f)(mb)
        val rb = foldMap(r)(f)(mb)
        mb.op(lb, rb)
    }
}

object OptionFoldable extends Foldable[Option] {
  // Exercise 10.14
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B =
    as match {
      case None => z
      case Some(a) => f(a, z)
    }

  // Exercise 10.14
  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B =
    as match {
      case None => z
      case Some(a) => f(z, a)
    }

  // Exercise 10.14
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
    as match {
      case None => mb.zero
      case Some(a) => f(a)
    }
}
