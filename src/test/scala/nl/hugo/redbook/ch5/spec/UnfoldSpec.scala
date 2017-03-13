package nl.hugo.redbook.ch5.spec

import nl.hugo.redbook.Spec
import nl.hugo.redbook.ch5.{ Empty, Stream }

trait UnfoldSpec extends Spec {

  def unfold[A, S]: S => (S => Option[(A, S)]) => Stream[A]

  def unfoldTest(name: String) = name should {

    "construct an empty stream" in {
      val stream = unfold(42)(_ => None)
      stream should be(Empty)
    }

    "construct a stream with multiple elements" in {
      val stream = unfold("Hello, world!") {
        case "" => None
        case s => Some(s.head, s.tail)
      }
      stream.take(5).toList should be(List('H', 'e', 'l', 'l', 'o'))
    }

    "construct an infinite stream" in {
      val stream = unfold(1)(i => Some(i, i * 2))
      stream.take(9).toList should be(List(1, 2, 4, 8, 16, 32, 64, 128, 256))
    }
  }
}