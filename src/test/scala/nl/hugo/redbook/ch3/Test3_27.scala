package nl.hugo.redbook.ch3

import org.scalatest._

class Test3_27 extends WordSpec with Matchers {
  import Exercise27.depth

  "depth" should {

    "return the maximum depth of all paths of a non-empty tree (left case)" in {
      depth(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))) should be(2)
    }

    "return the maximum depths of all paths of a non-empty tree (right case)" in {
      depth(Branch(Leaf(3), Branch(Leaf(2), Leaf(1)))) should be(2)
    }

    "return the depth of a tree with one element" in {
      depth(Leaf(42)) should be(0)
    }
  }
}