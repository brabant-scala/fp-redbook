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
  val intAddition: Monoid[Int] = ???

  // Exercise 10.01
  val intMultiplication: Monoid[Int] = ???

  // Exercise 10.01
  val booleanOr: Monoid[Boolean] = ???

  // Exercise 10.01
  val booleanAnd: Monoid[Boolean] = ???

  // Exercise 10.02
  def optionMonoid[A]: Monoid[Option[A]] = ???

  // Exercise 10.03
  def endoMonoid[A]: Monoid[A => A] = ???

  import nl.hugo.redbook.ch8._

  // Exercise 10.04
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = ???

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  // Exercise 10.05
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = ???

  // Exercise 10.06
  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = ???

  // Exercise 10.06
  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = ???

  // Exercise 10.07
  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = ???

  // Exercise 10.08
  def par[A](m: Monoid[A]): Monoid[Par[A]] = ???

  // Exercise 10.08
  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = ???

  // Exercise 10.09
  def ordered(ints: IndexedSeq[Int]): Boolean = ???

  sealed trait WC

  case class Stub(chars: String) extends WC

  case class Part(lStub: String, words: Int, rStub: String) extends WC

  // Exercise 10.10
  // Note: The lazy was added to allow compilation without a concrete implementation.
  lazy val wcMonoid: Monoid[WC] = ???

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
  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B = ???

  // Exercise 10.12
  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B = ???

  // Exercise 10.12
  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B = ???

  // Exercise 10.12
  def concatenate[A](as: F[A])(m: Monoid[A]): A = ???

  // Exercise 10.15
  def toList[A](as: F[A]): List[A] = ???
}

object ListFoldable extends Foldable[List] {
  // Exercise 10.12
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = ???

  // Exercise 10.12
  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = ???

  // Exercise 10.12
  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B = ???
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  // Exercise 10.12
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B = ???

  // Exercise 10.12
  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B = ???

  // Exercise 10.12
  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B = ???
}

object StreamFoldable extends Foldable[Stream] {
  // Exercise 10.12
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B = ???

  // Exercise 10.12
  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B = ???
}

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  // Exercise 10.13
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = ???

  // Exercise 10.13
  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = ???

  // Exercise 10.13
  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = ???
}

object OptionFoldable extends Foldable[Option] {
  // Exerrcise 10.14
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B = ???

  // Exerrcise 10.14
  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B = ???

  // Exerrcise 10.14
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B = ???
}
