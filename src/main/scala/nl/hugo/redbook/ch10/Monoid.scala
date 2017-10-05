package nl.hugo.redbook.ch10

import nl.hugo.redbook.ch6.RNG
import nl.hugo.redbook.ch7.Nonblocking._
import nl.hugo.redbook.ch7.Nonblocking.Par._

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
    override def op(a1: Int, a2: Int) = a1 + a2

    override def zero = 0
  }

  // Exercise 10.01
  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2

    override def zero: Int = 1
  }

  // Exercise 10.01
  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2

    override def zero: Boolean = false
  }

  // Exercise 10.01
  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

    override def zero: Boolean = true
  }

  // Exercise 10.02
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] = (a1,a2) match {
      case (None, _) => a2
      case _         => a1
    }

    override def zero: Option[A] = None
  }

  // Exercise 10.03
  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(a1: A => A, a2: A => A): A => A = a => a2(a1(a))

    override def zero: A => A = x => x
  }

  // Copied from Ardjan
  def flipInputs[A](m: Monoid[A]) = new Monoid[A] {
    def op(a1: A, a2: A): A = m.op(a2, a1)

    val zero: A = m.zero
  }
  // Copied from Ardjan
  def reverseEndoMonoid[A]: Monoid[A => A] = flipInputs(endoMonoid)

  import nl.hugo.redbook.ch8._

  // Exercise 10.04
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = ???

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  // Exercise 10.05
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = as.map(f).foldRight(m.zero)(m.op)

  // Exercise 10.06
  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = ???
//    foldMap(as, new Monoid[(A, B) => B] {
//      override def op(a1: (A, B) => B, a2: (A, B) => B): (A, B) => B = ???
//
//      override def zero: (A, B) => B = (_, _) => z
//    })(f).apply()

//    foldMap(as, new Monoid[B] {
//      override def op(b1: B, b2: B): B = b1
//      override def zero: B = z
//    })(a => f(a, b1))

  // Exercise 10.06
  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = ???

  // Exercise 10.07
  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (as.length > 2) {
      val (left, right) = as.splitAt(as.length / 2)
      m.op(foldMapV(left, m)(f), foldMapV(right, m)(f))
    } else {
      as.map(f).foldRight(m.zero)(m.op)
    }
  }

  // Exercise 10.08
  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    override def op(a1: Par[A], a2: Par[A]): Par[A] = Par.map2(a1, a2)(m.op)

    override def zero: Par[A] = Par.unit(m.zero)
  }

  // Exercise 10.08
  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = {
    if (v.length > 2) {
      val (left, right) = v.splitAt(v.length / 2)
      Par.map2(parFoldMap(left, m)(f), parFoldMap(right, m)(f))(m.op)
    } else {
      Par.unit(v.map(f).foldRight(m.zero)(m.op))
    }
  }

  // Exercise 10.09
  def foldMap[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = as.map(f).foldRight(m.zero)(m.op)

  def ordered(ints: IndexedSeq[Int]): Boolean =
    foldMap(ints.sliding(2).collect { case IndexedSeq(x,y) => y - x }.toIndexedSeq, new Monoid[Boolean] {
      override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

      override def zero: Boolean = true
    })(_ >= 0)

  sealed trait WC
  // Stub does not contain spaces, but can be empty
  case class Stub(chars: String) extends WC

  // lStub and rStub do not contain spaces, but can be empty
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  // Exercise 10.10
  // Note: The lazy was added to allow compilation without a concrete implementation.
  lazy val wcMonoid: Monoid[WC] = new Monoid[WC] {
    override def op(a1: WC, a2: WC): WC = (a1, a2) match {
      case (Stub(chars1), Stub(chars2))                                 => Stub(chars1 + chars2)
      case (Stub(chars1), Part(lStub2, words2, rStub2))                 => Part(chars1 + lStub2, words2, rStub2         )
      case (Part(lStub1, words1, rStub1), Stub(chars2))                 => Part(lStub1         , words1, rStub1 + chars2)
      case (Part(lStub1, words1, rStub1), Part(lStub2, words2, rStub2)) => Part(lStub1         , words1 + words2 + (if ((rStub1 + lStub2).length > 0) 1 else 0), rStub2)
    }

    override def zero: WC = Part("", 0, "")
  }

  // Exercise 10.11
  def count(s: String): Int = ???

  // Exercise 10.16
  def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] = ???

  // Exercise 10.17
  def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] = ???

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
  def bag[A](as: IndexedSeq[A]): Map[A, Int] = ???
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
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

  // Exercise 10.12
  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

  // Exercise 10.12
  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B = as.foldRight(mb.zero)((a,b) => mb.op(f(a), b))
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  // Exercise 10.12
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

  // Exercise 10.12
  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

  // Exercise 10.12
  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B = as.foldRight(mb.zero)((a,b) => mb.op(f(a), b))
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
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = {
    def unfold(as: Tree[A], b: B): B = {
      as match {
        case Leaf(value) => mb.op(b, f(value))
        case Branch(left, right) =>
          val b2 = mb.op(b, unfold(left, b))
          mb.op(b2, unfold(right, b2))
      }
    }

    unfold(as, mb.zero)
  }

  // Exercise 10.13
  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = ???

  // Exercise 10.13
  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = ???
}

object OptionFoldable extends Foldable[Option] {
  // Exerrcise 10.14
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B = as.foldRight(mb.zero)((a,b) => mb.op(f(a), b))

  // Exerrcise 10.14
  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

  // Exerrcise 10.14
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)
}
