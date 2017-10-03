package nl.hugo.redbook.ch10

import org.scalatest.{ Matchers, WordSpec }

class Test10_12 extends WordSpec with Matchers with FoldableBehaviours {

  "ListFoldable" should {
    val lf: Foldable[List] = ListFoldable

    behave like aSeqFoldMapable(lf)
    behave like aSeqFoldRightable(lf)
    behave like aSeqFoldLeftable(lf)
    behave like aSeqConcatable(lf)
  }

  "StreamFoldable" should {
    val sf: Foldable[Stream] = StreamFoldable

    behave like aSeqFoldMapable(sf)
    behave like aSeqFoldRightable(sf)
    behave like aSeqFoldLeftable(sf)
    behave like aSeqConcatable(sf)
  }

  "IndexedSeqFoldable" should {
    val idxf: Foldable[IndexedSeq] = IndexedSeqFoldable

    behave like aSeqFoldMapable(idxf)
    behave like aSeqFoldRightable(idxf)
    behave like aSeqFoldLeftable(idxf)
    behave like aSeqConcatable(idxf)
  }

  // These two sets are to test the methods in Foldable[]
  object FoldableWithFoldLeft extends Foldable[List] {
    def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
      ListFoldable.foldLeft(as)(z)(f)
    def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
      foldMap(as)(f.curried)(Monoid.endoMonoid[B])(z)
  }

  "FoldableWithFoldLeft" should {
    val foldable = FoldableWithFoldLeft

    behave like aSeqFoldMapable(foldable)
    behave like aSeqFoldRightable(foldable)
    behave like aSeqFoldLeftable(foldable)
    behave like aSeqConcatable(foldable)
  }

  object FoldableWithFoldRight extends Foldable[List] {
    def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
      ListFoldable.foldRight(as)(z)(f)
    def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
      foldMap(as)(a => (b: B) => f(b, a))(Monoid.swap(Monoid.endoMonoid[B]))(z)
  }

  "FoldableWithFoldRight" should {
    val foldable = FoldableWithFoldLeft

    behave like aSeqFoldMapable(foldable)
    behave like aSeqFoldRightable(foldable)
    behave like aSeqFoldLeftable(foldable)
    behave like aSeqConcatable(foldable)
  }
}
