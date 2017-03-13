package nl.hugo.redbook.ch3

import org.scalatest._

class Test3_06 extends WordSpec with Matchers {
  import List.init

  "init" should {

    "return all but the tail for a list with multiple elements" in {
      init(List(1, 2, 3, 4, 5)) should be(List(1, 2, 3, 4))
    }

    "return Nil for list with one element" in {
      init(List(42)) should be(Nil)
    }

    "return Nil for empty list " in {
      init(Nil) should be(Nil)
    }

//    "throw an exception for an empty list" in {
//      intercept[Exception] {
//        init(Nil)
//      }
//    }
  }
}