package nl.hugo.redbook.ch3

import org.scalatest._

class Test3_11 extends WordSpec with Matchers {
  import Exercise11.{ sum, product }

  "sum" should {

    "calculate the sum of all elements of a list" in {
      sum(List(1, 2, 3, 4, 5)) should be(List.sum(List(1, 2, 3, 4, 5)))
    }

    "return 0 for an empty list" in {
      sum(Nil) should be(List.sum(Nil))
    }
  }

  "product" should {

    "calculate the product of all elements of a list" in {
      product(List(1.0, 2.0, 3.0)) should be(List.product(List(1.0, 2.0, 3.0)))
    }

    "return 1.0 for an empty list" in {
      product(Nil) should be(List.product(Nil))
    }
  }
}