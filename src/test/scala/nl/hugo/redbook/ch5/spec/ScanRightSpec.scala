package nl.hugo.redbook.ch5.spec

import nl.hugo.redbook.Spec
import nl.hugo.redbook.ch5.Stream
import nl.hugo.redbook.ch5.util.Eval

trait ScanRightSpec extends Spec {

  def scanRight[A, B](s: Stream[A]): B => ((A, => B) => B) => Stream[B]

  def scanRightTest(name: String) = name should {

    "foldRight a non-empty stream and return all intermediate results" in {
      val eval = Eval()
      val stream = eval.stream(1, 2, 3)
      val result = scanRight(stream)(0)(_ + _)
      eval.expect(0)
      result.toList should be(List(6, 5, 3, 0))
      eval.expect(3)
    }

    "return empty for an empty stream" in {
      val stream = Stream.empty[Int]
      scanRight(stream)(42)(_ + _).toList should be(List(42))
    }
  }
}