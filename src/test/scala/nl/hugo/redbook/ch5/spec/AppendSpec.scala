package nl.hugo.redbook.ch5.spec

import nl.hugo.redbook.Spec
import nl.hugo.redbook.ch5.{ Empty, Stream }
import nl.hugo.redbook.ch5.util.Eval

trait AppendSpec extends Spec {

  def append[A, B >: A](s: Stream[A]): (=> Stream[B]) => Stream[B]

  def appendTest(name: String) = name should {

    "concatenate two non-empty streams" in {
      val eval = Eval()
      val stream1 = eval.stream(1, 2, 3)
      val stream2 = eval.stream(4, 5, 6)
      val result = append(stream1)(stream2)
      eval.expect(0)
      result.toList should be(List(1, 2, 3, 4, 5, 6))
      eval.expect(6)
    }

    "concatenate a non-empty and an empty stream" in {
      val eval = Eval()
      val stream = eval.stream(1, 2, 3)
      val result = append(stream)(Empty)
      eval.expect(0)
      result.toList should be(List(1, 2, 3))
      eval.expect(3)
    }

    "concatenate an empty and a non-empty stream" in {
      val eval = Eval()
      val stream = eval.stream(1, 2, 3)
      val result = append(Empty)(stream)
      eval.expect(0)
      result.toList should be(List(1, 2, 3))
      eval.expect(3)
    }

    "concatenate two empty streams" in {
      val stream = Stream.empty[Boolean]
      append(stream)(Empty) should be(Empty)
    }
  }
}