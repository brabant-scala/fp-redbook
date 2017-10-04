package red_book_exercises.chapter_10_monoids

import red_book_exercises.chapter_10_monoids.MonoidsExercise.Foldable.IndexedSeqFoldable
import red_book_exercises.chapter_3_functional_data_structures.{Branch, Leaf, Tree}

trait Monoid[A] {
  def op(a: A, b: A): A

  def zero: A

  def dual: Monoid[A] = new Monoid[A] {
    override def op(a: A, b: A) = dual.op(b, a)

    override def zero = dual.zero
  }
}

object MonoidsExercise {


  //<editor-fold desc="10.1">
  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(a: Int, b: Int): Int = a + b

    override def zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def op(a: Int, b: Int): Int = a * b

    override def zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a: Boolean, b: Boolean): Boolean = a || b

    override def zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a: Boolean, b: Boolean): Boolean = a && b

    override def zero: Boolean = true
  }
  //</editor-fold>

  //<editor-fold desc="10.2">
  def optionMonoid[A: Monoid]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(a: Option[A], b: Option[A]) = for {
      aa <- a
      bb <- b
    } yield implicitly[Monoid[A]].op(aa, bb)

    override def zero = None
  }

  //</editor-fold>

  //<editor-fold desc="10.3">
  def endoMonoid[A]: Monoid[A => A] = new Monoid[(A) => A] {

    override def op(a: (A) => A, b: (A) => A): (A) => A = a compose b

    override def zero: (A) => A = identity
  }

  //</editor-fold>

  //<editor-fold desc="10.4">
  /* Scalacheck in test
  def identityLaw[A](m: Monoid[A], gen: Gen[A]) = forAll(gen, { a: A =>
    m.op(a, m.zero) == a && m.op(m.zero, a) == a
  })

  def associativeLaw[A](m: Monoid[A], gen: Gen[A]) = forAll(gen, gen, gen, { (a1: A, a2: A, a3: A) =>
    m.op(m.op(a1, a2), a3) == m.op(a1, m.op(a2, a3))
  })*/
  //</editor-fold>

  //<editor-fold desc="Description">
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = as.map(f).fold(m.zero)(m.op)

  //</editor-fold>


  //<editor-fold desc="10.6">
  def foldLeftByFoldMap[A, B](as: List[A]): B = ???

  def foldRightByFoldMap[A, B](as: List[A]): B = ???

  //</editor-fold>

  //<editor-fold desc="10.7">
  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (v.isEmpty) m.zero
    else if (v.length == 1) f(v(0))
    else {
      val s = v.splitAt(v.length / 2)
      m.op(foldMapV(s._1, m)(f), foldMapV(s._2, m)(f))
    }
  }

  //</editor-fold>


  //<editor-fold desc="10.8">
  //Didn't do Chapter 7
  //</editor-fold>

  //<editor-fold desc="10.9">
  def ordered(v: IndexedSeq[Int]): Boolean = {
    /*
     * To make the ordering monoid associative we must compare the max of the left side to the minimum of the right side
     * i.e. (1, 2, 1, 3) -> if we would only compare max of (1, 2) and (1, 3) it would return true
     * and if we would compare only minimum of (1, 2) and (1, 3) it would also return true. So we must compare both.
     */
    val monoid: Monoid[Option[(Int, Int, Boolean)]] = new Monoid[Option[(Int, Int, Boolean)]] {
      override def op(a: Option[(Int, Int, Boolean)], b: Option[(Int, Int, Boolean)]) = (a, b) match {
        case (Some((x1, y1, p1)), Some((x2, y2, p2))) => Some(x1 min x2, y1 max y2, p1 && p2 && y1 <= x2)
        case (x, None) => x
        case (None, x) => x
      }

      override def zero = None
    }

    foldMapV(v, monoid)(i => Some(i, i, true)).map(_._3).getOrElse(true)
  }
  //</editor-fold>

  //<editor-fold desc="10.10">
  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    override def op(a: WC, b: WC): WC = (a, b) match {
      case (Stub(w1), Stub(w2)) => Stub(w1 + w2)
      case (Stub(w1), Part(l2, i2, r2)) => Part(w1 + l2, i2, r2)
      case (Part(l1, i1, r1), Stub(w2)) => Part(l1, i1, r1 + w2)
      case (Part(l1, i1, r1), Part(l2, i2, r2)) => Part(l1, i1 + (if ((r1 + l2).isEmpty) 0 else 1) + i2, r2)
    }

    override def zero: WC = Stub("")
  }
  //</editor-fold>

  //<editor-fold desc="10.11">
  def count(s: String): Int = {
    def countStub(s: String): Int = s.length min 1

    val wc = foldMapV(s.toIndexedSeq, wcMonoid) { c =>
      if (c.isWhitespace) Part("", 0, "")
      else Stub(c.toString)
    }

    wc match {
      case Stub(str) => countStub(str)
      case Part(l, w, r) => countStub(l) + w + countStub(r)
    }
  }
  //</editor-fold>

  trait Foldable[F[_]] {
    def foldRight[A, B](as: F[A])(initial: B)(f: (A, B) => B): B =
      foldMap(as)(f.curried)(endoMonoid[B])(initial)

    def foldLeft[A, B](as: F[A])(initial: B)(f: (B, A) => B): B =
      foldMap(as)(a => (b: B) => f(b, a))(endoMonoid[B].dual)(initial)

    def foldMap[A, B](as: F[A])(f: A => B)(implicit mb: Monoid[B]): B =
      foldRight(as)(mb.zero)((a, b) => mb.op(f(a), b))

    def concatenate[A](as: F[A])(implicit m: Monoid[A]): A =
      foldLeft(as)(m.zero)(m.op)

    //<editor-fold desc="10.15">
    def toList[A](as: F[A]): List[A] = foldRight(as)(List[A]())(_ :: _)

    //</editor-fold>
  }

  //<editor-fold desc="10.12">
  //implicitly[Foldable[List]].concatenate(List(1,2,3)) = 6
  //implicitly[Foldable[IndexedSeq]].concatenate(IndexedSeq(1,2,3)) = 6
  //implicitly[Foldable[Stream]].concatenate(Stream(1,2,3)) = 6

  object Foldable {
    implicit object ListFoldable extends Foldable[List] {
      override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
        as.foldRight(z)(f)

      override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
        as.foldLeft(z)(f)

      override def foldMap[A, B](as: List[A])(f: A => B)(implicit mb: Monoid[B]): B =
        foldLeft(as)(mb.zero)((b, a) => mb.op(b, f(a)))
    }

    implicit object IndexedSeqFoldable extends Foldable[IndexedSeq] {

      override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B =
        as.foldRight(z)(f)

      override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B =
        as.foldLeft(z)(f)

      override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(implicit mb: Monoid[B]): B =
        foldMapV(as, mb)(f)
    }

    implicit object StreamFoldable extends Foldable[Stream] {
      override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B =
        as.foldRight(z)(f)

      override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B =
        as.foldLeft(z)(f)
    }
  }

  //<editor-fold desc="10.13">
  implicit object TreeFoldable extends Foldable[Tree] {
    override def foldMap[A, B](as: Tree[A])(f: (A) => B)(implicit mb: Monoid[B]): B =
      Tree.fold(as)(f)(mb.op)

    override def foldLeft[A, B](as: Tree[A])(initial: B)(f: (B, A) => B): B = as match {
      case Leaf(a) => f(initial, a)
      case Branch(l, r) => foldLeft(r)(
        foldLeft(l)(initial)(f)
      )(f)
    }

    override def foldRight[A, B](as: Tree[A])(initial: B)(f: (A, B) => B): B = as match {
      case Leaf(a) => f(a, initial)
      case Branch(l, r) => foldRight(l)(
        foldRight(r)(initial)(f)
      )(f)
    }
    //</editor-fold>

    //<editor-fold desc="10.14">
    implicit object OptionFoldable extends Foldable[Option] {
      override def foldMap[A, B](as: Option[A])(f: (A) => B)(implicit mb: Monoid[B]): B =
        as.fold(mb.zero)(f)

      override def foldLeft[A, B](as: Option[A])(initial: B)(f: (B, A) => B): B =
        as.fold(initial)(a => f(initial, a))

      override def foldRight[A, B](as: Option[A])(initial: B)(f: (A, B) => B): B =
        as.fold(initial)(a => f(a, initial))
    }
    //</editor-fold>


  }
  //</editor-fold>

  //<editor-fold desc="10.16">
  def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    def op(a: (A, B), b: (A, B)): (A, B) = (A.op(a._1, b._1), B.op(a._2, b._2))

    override def zero: (A, B) = (A.zero, B.zero)
  }

  //</editor-fold>

  //<editor-fold desc="10.17">
  def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] = new Monoid[(A) => B] {
    override def op(a: (A) => B, b: (A) => B): (A) => B = i => B.op(a(i), b(i))

    override def zero: (A) => B = _ => B.zero
  }

  //</editor-fold>

  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
    def zero = Map[K, V]()

    def op(a: Map[K, V], b: Map[K, V]) = (a.keySet ++ b.keySet).foldLeft(zero) { (acc, k) =>
      acc.updated(k, V.op(
        a.getOrElse(k, V.zero),
        b.getOrElse(k, V.zero)
      ))
    }
  }

  //<editor-fold desc="10.18">
  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    IndexedSeqFoldable.foldMap(as)(a => Map(a -> 1))(mapMergeMonoid(intAddition))
  //</editor-fold>

}
