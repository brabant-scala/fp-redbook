package nl.hugo.redbook.ch5.spec

import nl.hugo.redbook.Spec
import nl.hugo.redbook.ch5.{ Empty, Stream }
import nl.hugo.redbook.ch5.util.Eval

trait ZipAllSpec extends Spec {

  def zipAll[A, B](s: Stream[A]): Stream[B] => Stream[(Option[A], Option[B])]

  def zipAllTest[A](name: String) = name should {

    "zip a stream with a stream with equal length" in {
      val eval1 = Eval()
      val eval2 = Eval()
      val stream1 = eval1.stream(1, 2, 3)
      val stream2 = eval2.stream(3, 4, 5)
      val result = zipAll(stream1)(stream2)
      eval1.expect(1)
      eval2.expect(1)
      result.toList should be(List(
        (Some(1), Some(3)),
        (Some(2), Some(4)),
        (Some(3), Some(5))
      ))
      eval1.expect(3)
      eval2.expect(3)
    }

    "zip a stream with a longer stream" in {
      val eval1 = Eval()
      val eval2 = Eval()
      val stream1 = eval1.stream("Hello", "world")
      val stream2 = eval2.stream(3, 4, 5)
      val result = zipAll(stream1)(stream2)
      eval1.expect(1)
      eval2.expect(1)
      result.toList should be(List(
        (Some("Hello"), Some(3)),
        (Some("world"), Some(4)),
        (None, Some(5))
      ))
      eval1.expect(2)
      eval2.expect(3)
    }

    "zip a stream with a shorter stream" in {
      val eval1 = Eval()
      val eval2 = Eval()
      val stream1 = eval1.stream(1, 2, 3)
      val stream2 = eval2.stream(true)
      val result = zipAll(stream1)(stream2)
      eval1.expect(1)
      eval2.expect(1)
      result.toList should be(List(
        (Some(1), Some(true)),
        (Some(2), None),
        (Some(3), None)
      ))
      eval1.expect(3)
      eval2.expect(1)
    }

    "zip a stream with an empty stream" in {
      val eval = Eval()
      val stream1 = eval.stream(1, 2, 3)
      val stream2 = Stream.empty[String]
      val result = zipAll(stream1)(stream2)
      eval.expect(1)
      result.toList should be(List(
        (Some(1), None),
        (Some(2), None),
        (Some(3), None)
      ))
      eval.expect(3)
    }

    "zip an empty stream with a stream" in {
      val eval = Eval()
      val stream1 = Stream.empty[Boolean]
      val stream2 = eval.stream(1, 2, 3)
      val result = zipAll(stream1)(stream2)
      eval.expect(1)
      result.toList should be(List(
        (None, Some(1)),
        (None, Some(2)),
        (None, Some(3))
      ))
      eval.expect(3)
    }

    "zip an empty strean with an empty stream" in {
      val stream1 = Stream.empty[String]
      val stream2 = Stream.empty[Boolean]
      val result = zipAll(stream1)(stream2)
      result should be(Empty)
    }
  }
}