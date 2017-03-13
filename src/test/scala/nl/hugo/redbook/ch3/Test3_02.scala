package nl.hugo.redbook.ch3

import org.scalatest._

class Test3_02 extends WordSpec with Matchers {
  import List.tail

  "tail" should {

    "return all but the head for a list with multiple elements" in {
      tail(List(1, 2, 3, 4, 5)) should be(List(2, 3, 4, 5))
    }

    "return Nil for list with one element" in {
      tail(List(42)) should be(Nil)
    }

    "return Nil for empty list" in {
      tail(Nil) should be(Nil)
    }

//    "throw an exception for an empty list" in {
//      intercept[Exception] {
//      }
//    }
  }
}