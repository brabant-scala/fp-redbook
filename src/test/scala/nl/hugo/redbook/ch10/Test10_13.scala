package nl.hugo.redbook.ch10

import org.scalatest.{ Matchers, WordSpec }

class Test10_13 extends WordSpec with Matchers with FoldableBehaviours {
  "TreeFoldable" should {
    val tf: Foldable[Tree] = TreeFoldable

    behave like aTreeFoldMapable(tf)
    behave like aTreeFoldRightable(tf)
    behave like aTreeFoldLeftable(tf)
    behave like aTreeConcatable(tf)
  }
}

