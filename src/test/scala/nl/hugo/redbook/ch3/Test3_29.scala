package nl.hugo.redbook.ch3

import org.scalatest._

class Test3_29 extends WordSpec with Matchers {
  import Tree.{ depth, map, maximum }

  "size" should {

    "return the number of leafs in a non-empty tree" in {
      Tree.size(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))) should be(7)
    }

    "return 1 for a tree with one element" in {
      Tree.size(Leaf(42)) should be(1)
    }
  }

  "maximum" should {

    "return the maximum value of all leafs of a non-empty tree (left case)" in {
      maximum(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))) should be(4)
    }

    "return the maximum value of all leafs of a non-empty tree (right case)" in {
      maximum(Branch(Branch(Leaf(4), Leaf(3)), Branch(Leaf(2), Leaf(1)))) should be(4)
    }

    "return the value of the leaf of a tree with one element" in {
      maximum(Leaf(42)) should be(42)
    }
  }

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

  "map" should {

    "return a new tree with a function applied to the values of all leafs of a non-empty tree" in {
      map(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)))(_ * 2) should be(Branch(Branch(Leaf(2), Leaf(4)), Leaf(6)))
    }

    "return a new tree with a funcion applied to the value of the leaf of a tree with one element" in {
      map(Leaf(42))(_ / 6) should be(Leaf(7))
    }
  }
}