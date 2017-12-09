package nl.hugo.redbook.ch3

import org.scalatest._

class Test3_23 extends WordSpec with Matchers {
  import List.zipWith

  "zipWith" should {

    "zip the elements of two lists and apply a function into a new list" in {
      zipWith(List(1, 2, 3), List(4.0, 5.0, 6.0))(_ * _) should be(List(4.0, 10.0, 18.0))
    }

    "zip the elements of two lists if the first list is shorter" in {
      zipWith(List("a", "b"), List("A", "B", "C"))(_ + _) should be(List("aA", "bB"))
    }

    "add the elements of two lists if the first list is longer" in {
      zipWith(List(1, 2, 3, 4), List(3, 2, 1))(_ < _) should be(List(true, false, false))
    }

    "return an empty list if the first list is empty" in {
      zipWith(Nil: List[Boolean], List(true, false))(_ == _) should be(Nil)
    }

    "return an empty list if the second list is empty" in {
      zipWith(List(1, 2, 3, 4), Nil)(_ + _) should be(Nil)
    }

    "return an empty list for an empty list" in {
      zipWith(Nil: List[Int], Nil)(_ + _) should be(Nil)
    }
  }
}