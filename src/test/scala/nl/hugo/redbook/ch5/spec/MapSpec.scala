package nl.hugo.redbook.ch5.spec

import nl.hugo.redbook.Spec
import nl.hugo.redbook.ch5.{ Empty, Stream }
import nl.hugo.redbook.ch5.util.Eval

trait MapSpec extends Spec {

  def map[A, B](s: Stream[A]): (A => B) => Stream[B]

  def mapTest[A](name: String) = name should {

    "convert each element of a stream to the same type" in {
      val eval = Eval()
      val stream = eval.stream(1, 2, 3, 4, 5)
      val result = map(stream)(_ + 1)
      eval.expect(0)
      result.toList should be(List(2, 3, 4, 5, 6))
      eval.expect(5)
    }

    "convert each element of a stream to a different type" in {
      val eval = Eval()
      val stream = eval.stream(1.0, 2.0, 3.0)
      val result = map(stream)(_.toString)
      eval.expect(0)
      result.toList should be(List("1.0", "2.0", "3.0"))
      eval.expect(3)
    }

    "convert each element of an infinite stream" in {
      val eval = Eval()
      def stream: Stream[Int] = Stream.cons(eval(42), stream)
      val result = map(stream)(_ / 6)
      eval.expect(0)
      result.take(3).toList should be(List(7, 7, 7))
    }

    "return an empty stream for an empty stream" in {
      val stream = Stream.empty[Int]
      map(stream)(_ * 2) should be(Empty)
    }
  }
}