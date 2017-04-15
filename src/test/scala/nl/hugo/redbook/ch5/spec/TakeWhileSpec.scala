package nl.hugo.redbook.ch5.spec

import nl.hugo.redbook.Spec
import nl.hugo.redbook.ch5.{ Empty, Stream }
import nl.hugo.redbook.ch5.util.Eval

trait TakeWhileSpec extends Spec {

  def takeWhile[A](s: Stream[A]): (A => Boolean) => Stream[A]

  def takeWhileTest[A](name: String) = name should {

    "return empty when the first element fails the predicate" in {
      val eval = Eval()
      val stream = eval.stream(1, 2, 3)
      takeWhile(stream)(_ == 0) should be(Empty)
      eval.expect(1)
    }

    "return all elements that satisfy the predicate" in {
      val eval = Eval()
      val stream = eval.stream(1, 2, 3)
      val result = takeWhile(stream)(_ < 4)
      eval.expect(1)
      result.toList should be(List(1, 2, 3))
      eval.expect(3)
    }

    "return all elements until the first that does not satisfy the predicate" in {
      val eval = Eval()
      val stream = eval.stream(1, 2, 3)
      val result = takeWhile(stream)(_ < 2)
      eval.expect(1)
      result.toList should be(List(1))
      eval.expect(2)
    }

    "return no elements after the first that does not satisfy the predicate" in {
      val eval = Eval()
      val stream = eval.stream(1, 2, 3)
      val result = takeWhile(stream)(_ % 2 == 1)
      eval.expect(1)
      result.toList should be(List(1))
      eval.expect(2)
    }

    "return empty for an empty stream" in {
      val stream = Stream.empty[Boolean]
      takeWhile(stream)(identity) should be(Empty)
    }
  }
}