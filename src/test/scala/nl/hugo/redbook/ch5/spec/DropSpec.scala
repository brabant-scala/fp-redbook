package nl.hugo.redbook.ch5.spec

import nl.hugo.redbook.Spec
import nl.hugo.redbook.ch5.{ Empty, Stream }
import nl.hugo.redbook.ch5.util.Eval

trait DropSpec extends Spec {

  def drop[A](s: Stream[A]): Int => Stream[A]

  def dropTest(name: String) = name should {

    "drop the first element when dropping one element" in {
      val eval = Eval()
      val stream = eval.stream(1, 2, 3, 4, 5)
      val result = drop(stream)(1)
      eval.expect(0)
      result.toList should be(List(2, 3, 4, 5))
      eval.expect(4)
    }

    "drop multiple elements" in {
      val eval = Eval()
      val stream = eval.stream(1, 2, 3, 4, 5)
      val result = drop(stream)(3)
      eval.expect(0)
      result.toList should be(List(4, 5))
      eval.expect(2)
    }

    "drop elements from an infinite stream" in {
      val eval = Eval()
      def stream: Stream[Int] = Stream.cons(eval(42), stream)
      val result = drop(stream)(3)
      eval.expect(0)
      result.take(3).toList should be(List(42, 42, 42))
      eval.expect(3)
    }

    "return the stream itself when dropping no elements" in {
      val eval = Eval()
      val stream = eval.stream(1, 2, 3)
      val result = drop(stream)(0)
      result should be(stream)
      eval.expect(0)
      result.toList should be(List(1, 2, 3))
      eval.expect(3)
    }

    "return an empty stream when dropping all elements" in {
      val eval = Eval()
      val stream = eval.stream(1, 2, 3)
      drop(stream)(3) should be(Empty)
      eval.expect(0)
    }

    "return an empty stream when dropping more elements than the stream contains" in {
      val eval = Eval()
      val stream = eval.stream(1, 2, 3)
      drop(stream)(4) should be(Empty)
      eval.expect(0)
    }

    "return an empty stream for an empty stream" in {
      drop(Empty)(1) should be(Empty)
    }
  }
}