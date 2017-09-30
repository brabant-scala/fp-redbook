package nl.hugo.redbook.ch10

import org.scalatest.{ Matchers, WordSpec }

class Test10_14 extends WordSpec with Matchers with FoldableBehaviours {
  "TreeFoldable" should {
    val of: Foldable[Option] = OptionFoldable

    behave like anOptionFoldMapable(of)
    behave like anOptionFoldRightable(of)
    behave like anOptionFoldLeftable(of)
    behave like anOptionConcatable(of)
  }
}

