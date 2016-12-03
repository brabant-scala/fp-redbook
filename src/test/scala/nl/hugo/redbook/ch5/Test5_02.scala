package nl.hugo.redbook.ch5

import org.scalatest._

class Test5_02 extends WordSpec with Matchers {
  "A Stream with four elements" should {
    "return Empty with no elements taken" in {
      Stream(1, 2, 3, 4).take(0) should be(Empty)
    }

    "return two Elements with two elements taken" in {
      Stream(1, 2, 3, 4).take(2).toList should be(List(1, 2))
    }

    "return four Elements with two elements taken" in {
      Stream(1, 2, 3, 4).take(4).toList should be(List(1, 2, 3, 4))
    }

    "return four Elements with five elements taken" in {
      Stream(1, 2, 3, 4).take(5).toList should be(List(1, 2, 3, 4))
    }

    "return four Elements with no elements dropped" in {
      Stream(1, 2, 3, 4).drop(0).toList should be(List(1, 2, 3, 4))
    }

    "return two Elements with two elements dropped" in {
      Stream(1, 2, 3, 4).drop(2).toList should be(List(3, 4))
    }

    "return Empty with four elements dropped" in {
      Stream(1, 2, 3, 4).drop(4).toList should be(List.empty)
    }

    "return Empty with five elements dropped" in {
      Stream(1, 2, 3, 4).drop(5).toList should be(List.empty)
    }
  }

  "An Empty" should {
    "return Empty with no elements taken" in {
      Empty.take(0) should be(Empty)
    }

    "return Empty with one element taken" in {
      Empty.take(1) should be(Empty)
    }

    "return Empty with no elements dropped" in {
      Empty.drop(0) should be(Empty)
    }

    "return Empty with one element dropped" in {
      Empty.drop(1) should be(Empty)
    }
  }
}

