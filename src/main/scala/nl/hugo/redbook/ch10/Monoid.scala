package nl.hugo.redbook.ch10

import scala.language.higherKinds
import nl.hugo.redbook.ch7.Nonblocking._
import nl.hugo.redbook.ch7.Nonblocking.Par._

import scala.annotation.tailrec
import scala.collection.immutable.Stream.Empty

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
    val zero: List[A] = Nil
  }

  // Exercise 10.01
  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2
    override def zero: Int = 0
  }

  // Exercise 10.01
  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int) = a1 * a2
    override def zero = 1
  }

  // Exercise 10.01
  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean) = a1 || a2
    override def zero = false
  }

  // Exercise 10.01
  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean) = a1 && a2
    override def zero = true
  }

  // Exercise 10.02
  def optionMonoid[A: Monoid]: Monoid[Option[A]] = new Monoid[Option[A]] {
    val M = implicitly[Monoid[A]]
    override def op(a1: Option[A], a2: Option[A]) = {
      for {
        v1 <- a1
        v2 <- a2
      } yield M.op(v1, v2)
    }
    override val zero = Some(M.zero)
  }

  // Exercise 10.03
  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(a1: A => A, a2: A => A): A => A = x => a2(a1(x))
    override def zero: A => A = x => x
  }

  def contraEndoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(a1: A => A, a2: A => A): A => A = x => a1(a2(x))
    override def zero: A => A = x => x
  }

  import nl.hugo.redbook.ch8._

  // Exercise 10.04
  def triples[A](gen: Gen[A]): Gen[(A, A, A)] = {
    for {
      v1 <- gen
      v2 <- gen
      v3 <- gen
    } yield (v1, v2, v3)
  }

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = {
    val identity: Prop = Prop.forAll(gen)((a: A) => (m.op(a, m.zero) == a) && (m.op(m.zero, a) == a))
    val assoc: Prop = Prop.forAll(triples(gen)) { case (a, b, c) => m.op(a, m.op(b, c)) == m.op(m.op(a, b), c) }

    identity && assoc
  }

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  // Exercise 10.05
  //  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = concatenate(as.map(f), m)
  //  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = as.foldRight(m.zero)((a, b) => m.op(f(a), b))

  // Exercise 10.06
  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = {
    foldMap[A, B => B](as, contraEndoMonoid[B])(f.curried)(z)
  }

  // Exercise 10.06
  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = {
    foldMap[A, B => B](as, endoMonoid[B]) { (a: A) => (b: B) => f(b, a) }(z)
  }

  // Exercise 10.07
  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    def foldMapV(index: Int, size: Int): B = {
      if (size == 0) {
        m.zero
      } else if (size == 1) {
        f(as(index))
      } else {
        val middleIndex = index + size / 2
        m.op(
          foldMapV(index, middleIndex - index),
          foldMapV(middleIndex, (index + size - middleIndex))
        )
      }
    }
    foldMapV(0, as.length)
  }

  // Exercise 10.08
  def par[A](m: Monoid[A]): Monoid[Par[A]] = ???

  // Exercise 10.08
  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = ???

  // Exercise 10.09
  def ordered(ints: IndexedSeq[Int]): Boolean = ???

  def isWhitespace(c: Char) = c.isWhitespace

  sealed trait WC

  case class Stub(chars: String) extends WC {
    require(chars.find(isWhitespace).isEmpty)
  }
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  // Exercise 10.10
  // Note: The lazy was added to allow compilation without a concrete implementation.
  lazy val wcMonoid: Monoid[WC] = new Monoid[WC] {
    override def op(a1: WC, a2: WC): WC = (a1, a2) match {
      case (Stub(str1), Stub(str2)) => Stub(str1 + str2)
      case (Stub(str), Part(lstub, n, rstub)) => Part(str + lstub, n, rstub)
      case (Part(lstub, n, rstub), Stub(str)) => Part(lstub, n, rstub + str)
      case (Part(lstub1, n1, ""), Part("", n2, rstub2)) => {
        Part(lstub1, n1 + n2, rstub2)
      }
      case (Part(lstub1, n1, rstub1), Part(lstub2, n2, rstub2)) => {
        Part(lstub1, n1 + 1 + n2, rstub2)
      }
    }
    override def zero: WC = Stub("")
  }

  // Exercise 10.11
  def count(s: String): Int = {
    foldMapV(
      (" " + s + " ").map {
        c =>
          if (c.isWhitespace) {
            Part("", 0, "")
          } else {
            Stub(c.toString)
          }
      }, wcMonoid
    )(x => x) match {
        case Stub(_) => 1
        case Part(_, n, _) => n
      }
  }

  // Exercise 10.16
  def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] = {
    new Monoid[(A, B)] {
      override def op(a1: (A, B), a2: (A, B)): (A, B) = (A.op(a1._1, a2._1), B.op(a1._2, a2._2))
      override def zero: (A, B) = (A.zero, B.zero)
    }
  }

  // Exercise 10.17
  def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] = {
    new Monoid[A => B] {
      override def op(f: A => B, g: A => B): A => B = {
        a => B.op(f(a), g(a))
      }

      override def zero: A => B = _ => B.zero
    }
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

  //  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {

  // Exercise 10.18
  def bag[A](as: IndexedSeq[A]): Map[A, Int] = {
    foldMapV[A, Map[A, Int]](as, mapMergeMonoid[A, Int](intAddition)) {
      a => Map(a -> 1)
    }

  }
}

trait Foldable[F[_]] {

  import Monoid._

  // Exercise 10.12
  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B = {
    type C = B => B
    foldLeft[A, C](as)(x => z) { (c: B => B, a: A) => (x: B) => f(a, c(x)) }(z)
  }

  // Exercise 10.12
  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B = {
    type C = B => B
    foldRight[A, C](as)(x => x) { (a: A, c: C) => (b: B) =>
      f(c(b), a)
    }(z)
  }

  // Exercise 10.12
  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B = foldLeft(as)(mb.zero)((b, a) => mb.op(b, f(a)))

  // Exercise 10.12
  def concatenate[A](as: F[A])(m: Monoid[A]): A = foldMap(as)(x => x)(m)

  // Exercise 10.15
  def toList[A](as: F[A]): List[A] = foldMap(as)(List(_))(listMonoid)
}

object ListFoldable extends Foldable[List] {
  // Exercise 10.12
  // not stack-safe ...
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = {
    as match {
      case Nil => z
      case h :: tail => f(h, foldRight(tail)(z)(f))
    }
  }

  // Exercise 10.12
  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = {
    @tailrec
    def helper(z: B, l: List[A]): B = {
      l match {
        case Nil => z
        case h :: tail => helper(f(z, h), tail)
      }
    }
    helper(z, as)
  }

  // Exercise 10.12
  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B = {
    foldLeft(as)(mb.zero) {
      (b, a) => mb.op(b, f(a))
    }
  }
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  // Exercise 10.12
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B = {
    as.foldRight(z)(f)
  }

  // Exercise 10.12
  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B = {
    as.foldLeft(z)(f)
  }

  // Exercise 10.12
  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B = {
    foldLeft(as)(mb.zero) {
      (b, a) => mb.op(b, f(a))
    }
  }
}

object StreamFoldable extends Foldable[Stream] {
  // Exercise 10.12
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B = {
    as match {
      case Empty => z
      case head #:: tail => f(head, foldRight(tail)(z)(f))
    }
  }

  // Exercise 10.12
  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B = {
    as match {
      case Empty => z
      case head #:: tail => foldLeft(tail)(f(z, head))(f)
    }
  }
}

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  // Exercise 10.13
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = {
    as match {
      case Leaf(value) => f(value)
      case Branch(left, right) => mb.op(
        foldMap(left)(f)(mb),
        foldMap(right)(f)(mb)
      )
    }
  }

  // Exercise 10.13
  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = {
    as match {
      case Leaf(value) => f(z, value)
      case Branch(left, right) => {
        val l = foldLeft(left)(z)(f)
        foldLeft(right)(l)(f)
      }
    }
  }

  // Exercise 10.13
  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = {
    as match {
      case Leaf(value) => f(value, z)
      case Branch(left, right) => {
        val r = foldRight(right)(z)(f)
        foldRight(left)(r)(f)
      }
    }
  }
}

object OptionFoldable extends Foldable[Option] {
  // Exerrcise 10.14
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B = {
    as match {
      case None => mb.zero
      case Some(value) => f(value)
    }
  }

  // Exerrcise 10.14
  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B = {
    as match {
      case None => z
      case Some(value) => f(z, value)
    }
  }

  // Exerrcise 10.14
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B = {
    as match {
      case None => z
      case Some(value) => f(value, z)
    }
  }
}
