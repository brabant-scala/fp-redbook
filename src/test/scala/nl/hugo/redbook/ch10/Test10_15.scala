package nl.hugo.redbook.ch10

import org.scalatest.{ Matchers, WordSpec }

class Test10_15 extends WordSpec with Matchers with FoldableBehaviours {

  "ListFoldable" should {
    val lf: Foldable[List] = ListFoldable

    behave like aSeqToList(lf)
  }

  "StreamFoldable" should {
    val sf: Foldable[Stream] = StreamFoldable

    behave like aSeqToList(sf)
  }

  "IndexedSeqFoldable" should {
    val idxf: Foldable[IndexedSeq] = IndexedSeqFoldable

    behave like aSeqToList(idxf)
  }

  "TreeFoldable" should {
    val tf: Foldable[Tree] = TreeFoldable

    behave like aTreeToList(tf)
  }

  "OptionFoldable" should {
    val of: Foldable[Option] = OptionFoldable

    behave like anOptionToList(of)
  }
}

