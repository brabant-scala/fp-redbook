package nl.hugo.redbook.ch5

import org.scalatest.{ Matchers, WordSpec }

class Test5_02 extends WordSpec with Matchers {
  "A stream" should {
    "drop the first element when dropping one element" in {
      Stream(1, 2, 3, 4, 5).drop(1).toList should be(List(2, 3, 4, 5))
    }

    "drop multiple elements" in {
      Stream(1, 2, 3, 4, 5).drop(3).toList should be(List(4, 5))
    }

    "drop elements from an infinite stream" in {
      Stream.constant(42).drop(3).take(3).toList should be(List(42, 42, 42))
    }

    "return the stream itself when dropping no elements" in {
      Stream(1, 2, 3).drop(0).toList should be(List(1, 2, 3))
    }

    "return an empty stream when dropping all elements" in {
      Stream(1, 2, 3).drop(3) should be(Empty)
    }

    "return an empty stream when dropping more elements than the stream contains" in {
      Stream(1, 2, 3).drop(4) should be(Empty)
    }

    "return an empty stream when dropping from an empty stream" in {
      Empty.drop(1) should be(Empty)
    }

    "return the first element when taking one element" in {
      Stream(1, 2, 3).take(1).toList should be(List(1))
    }

    "take multiple elements" in {
      Stream(1, 2, 3, 4, 5).take(3).toList should be(List(1, 2, 3))
    }

    "take multiple elements from an infinite stream" in {
      Stream.constant(42).take(3).toList should be(List(42, 42, 42))
    }

    "return the stream itself when taking all elements" in {
      Stream(1, 2, 3).take(3).toList should be(List(1, 2, 3))
    }

    "return the stream itself when taking more elements than the stream contains" in {
      Stream(1, 2, 3).take(4).toList should be(List(1, 2, 3))
    }

    "return an empty stream itself when taking no elements" in {
      Stream(1, 2, 3).take(0) should be(Empty)
    }

    "return an empty stream when taking from an empty stream" in {
      Empty.take(1) should be(Empty)
    }
  }
}