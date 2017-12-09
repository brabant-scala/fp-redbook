package nl.hugo.redbook.ch5.spec

import nl.hugo.redbook.ch5.{ Empty, Stream }
import org.scalatest.{ FlatSpec, Matchers, WordSpec }

trait TakeWhileBehavior {
  this: FlatSpec with Matchers =>

  def aTakeWhileFunction(f: Stream[Int] => (Int => Boolean) => Stream[Int]) {
    it should "return empty when the first element fails the predicate" in {
      f(Stream(1, 2, 3))(_ == 0) should be(Empty)
    }

    it should "return all elements that satisfy the predicate" in {
      f(Stream(1, 2, 3))(_ < 4).toList should be(List(1, 2, 3))
    }

    it should "return all elements until the first that does not satisfy the predicate" in {
      f(Stream(1, 2, 3))(_ < 2).toList should be(List(1))
    }

    it should "return no elements after the first that does not satisfy the predicate" in {
      f(Stream(1, 2, 3))(_ % 2 == 1).toList should be(List(1))
    }

    it should "return empty for an empty stream" in {
      f(Stream.empty[Int])(_ => true) should be(Empty)
    }
  }
}