package nl.hugo.redbook.ch5.spec

import nl.hugo.redbook.Spec
import nl.hugo.redbook.ch5.{ Empty, Stream }
import nl.hugo.redbook.ch5.util.Eval

trait ZipWithSpec extends Spec {

  def zipWith[A, B, C](s: Stream[A]): Stream[B] => ((A, B) => C) => Stream[C]

  def zipWithTest[A](name: String) = name should {

    "zip a stream with a stream with equal length" in {
      val eval1 = Eval()
      val eval2 = Eval()
      val stream1 = eval1.stream(1, 2, 3)
      val stream2 = eval2.stream(3, 4, 5)
      val result = zipWith(stream1)(stream2)(_ + _)
      eval1.expect(1)
      eval2.expect(1)
      result.toList should be(List(4, 6, 8))
      eval1.expect(3)
      eval2.expect(3)
    }

    "zip a stream with a longer stream" in {
      val eval1 = Eval()
      val eval2 = Eval()
      val stream1 = eval1.stream(1, 2, 3)
      val stream2 = eval2.stream(1, 2, 3, 4, 5)
      val result = zipWith(stream1)(stream2)(_ * _)
      eval1.expect(1)
      eval2.expect(1)
      result.toList should be(List(1, 4, 9))
      eval1.expect(3)
      eval2.expect(3)
    }

    "zip a stream with a shorter stream" in {
      val eval1 = Eval()
      val eval2 = Eval()
      val stream1 = eval1.stream(1, 2, 3)
      val stream2 = eval2.stream(1)
      val result = zipWith(stream1)(stream2)(_ - _)
      eval1.expect(1)
      eval2.expect(1)
      result.toList should be(List(0))
      eval1.expect(1)
      eval2.expect(1)
    }

    "zip a stream with an empty stream" in {
      val eval = Eval()
      val stream1 = eval.stream(1, 2, 3)
      val stream2 = Stream.empty[Int]
      zipWith(stream1)(stream2)(_ / _) should be(Empty)
      eval.expect(0)
    }

    "zip an empty stream with a stream" in {
      val eval = Eval()
      val stream1 = Stream.empty[Int]
      val stream2 = eval.stream(1, 2, 3)
      zipWith(stream1)(stream2)(_ + _) should be(Empty)
      eval.expect(0)
    }

    "zip an empty strean with an empty stream" in {
      val stream1 = Stream.empty[String]
      val stream2 = Stream.empty[Int]
      zipWith(stream1)(stream2)(_ * _) should be(Empty)
    }
  }
}