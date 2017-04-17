package nl.hugo.redbook.ch5.spec

import nl.hugo.redbook.Spec
import nl.hugo.redbook.ch5.{ Empty, Stream }
import nl.hugo.redbook.ch5.util.Eval

trait FlatMapSpec extends Spec {

  def flatMap[A, B](s: Stream[A]): (A => Stream[B]) => Stream[B]

  def flatMapTest[A](name: String) = name should {

    "combine the mapped results of multiple streams into a single stream" in {
      val eval1 = Eval()
      val eval2 = Eval()
      val stream = eval1.stream(1, 2, 3)
      val result = flatMap(stream)(i => eval2.stream(i, i))
      eval1.expect(1)
      eval2.expect(0)
      result.toList should be(List(1, 1, 2, 2, 3, 3))
      eval1.expect(3)
      eval2.expect(6)
    }

    "combine empty and non-empty streams into a single stream" in {
      val eval1 = Eval()
      val eval2 = Eval()
      def f(i: Int): Stream[Int] = if (i % 2 == 0) Empty else eval2.stream(i, i)
      val stream = eval1.stream(1, 2, 3)
      val result = flatMap(stream)(f)
      eval1.expect(1)
      eval2.expect(0)
      result.toList should be(List(1, 1, 3, 3))
      eval1.expect(3)
      eval2.expect(4)
    }

    "return an empty stream for an empty stream" in {
      val eval = Eval()
      val stream = Stream.empty[Boolean]
      flatMap(stream)(i => eval.stream(i, i)) should be(Empty)
      eval.expect(0)
    }
  }
}