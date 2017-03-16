package nl.hugo.redbook.ch5.spec

import nl.hugo.redbook.Spec
import nl.hugo.redbook.ch5.{ Empty, Stream }
import nl.hugo.redbook.ch5.util.Eval

trait TakeSpec extends Spec {

  def take[A](s: Stream[A]): Int => Stream[A]

  def takeTest(name: String) = name should {

    "return the first element when taking one element" in {
      val eval = Eval()
      val stream = eval.stream(1, 2, 3)
      val result = take(stream)(1)
      eval.expect(0)
      result.toList should be(List(1))
      eval.expect(1)
    }

    "take multiple elements" in {
      val eval = Eval()
      val stream = eval.stream(1, 2, 3, 4, 5)
      val result = take(stream)(3)
      eval.expect(0)
      result.toList should be(List(1, 2, 3))
      eval.expect(3)
    }

    "take multiple elements from an infinite stream" in {
      val eval = Eval()
      def stream: Stream[Int] = Stream.cons(eval(42), stream)
      val result = take(stream)(3)
      eval.expect(0)
      result.toList should be(List(42, 42, 42))
      eval.expect(3)
    }

    "return the stream itself when taking all elements" in {
      val eval = Eval()
      val stream = eval.stream(1, 2, 3)
      val result = take(stream)(3)
      eval.expect(0)
      result.toList should be(List(1, 2, 3))
      eval.expect(3)
    }

    "return the stream itself when taking more elements than the stream contains" in {
      val eval = Eval()
      val stream = eval.stream(1, 2, 3)
      val result = take(stream)(4)
      eval.expect(0)
      result.toList should be(List(1, 2, 3))
      eval.expect(3)
    }

    "return an empty stream itself when taking no elements" in {
      val eval = Eval()
      val stream = eval.stream(1, 2, 3)
      take(stream)(0) should be(Empty)
      eval.expect(0)
    }

    "return an empty stream for an empty stream" in {
      take(Empty)(1) should be(Empty)
    }
  }
}