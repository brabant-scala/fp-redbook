package nl.hugo.redbook.ch3

import org.scalatest._

class Test3_03 extends WordSpec with Matchers {
  import List.setHead

  "setHead" should {

    "change the head of a list with multiple elements" in {
      setHead(List(1, 2, 3, 4, 5), 7) should be(List(7, 2, 3, 4, 5))
    }

    "change the head of a list with one element" in {
      setHead(List(7), 42) should be(List(42))
    }

    "change the head of an empty list" in {
      setHead(Nil, 42) should be(Nil)
    }

//    "throw an exception for an empty list" in {
//      intercept[Exception] {
//        setHead(Nil, 13)
//      }
//    }
  }
}