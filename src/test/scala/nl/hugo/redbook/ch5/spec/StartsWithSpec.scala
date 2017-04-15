package nl.hugo.redbook.ch5.spec

import nl.hugo.redbook.Spec
import nl.hugo.redbook.ch5.Stream
import nl.hugo.redbook.ch5.util.Eval

trait StartsWithSpec extends Spec {

  def startsWith[A](s: Stream[A]): Stream[A] => Boolean

  def startsWithTest[A](name: String) = name should {

    "return true for two equal streams" in {
      val eval1 = Eval()
      val eval2 = Eval()
      val stream1 = eval1.stream(1, 2, 3)
      val stream2 = eval2.stream(1, 2, 3)
      startsWith(stream1)(stream2) should be(true)
      eval1.expect(3)
      eval2.expect(3)
    }

    "return true when the second stream is shorter" in {
      val eval1 = Eval()
      val eval2 = Eval()
      val stream1 = eval1.stream(1, 2, 3, 4, 5)
      val stream2 = eval2.stream(1, 2, 3)
      startsWith(stream1)(stream2) should be(true)
      eval1.expect(3)
      eval2.expect(3)
    }

    "return true when the second stream is empty" in {
      val eval = Eval()
      val stream1 = eval.stream(1, 2, 3)
      val stream2 = Stream.empty[Int]
      startsWith(stream1)(stream2) should be(true)
      eval.expect(0)
    }

    "return true when both streams are empty" in {
      val stream1 = Stream.empty[Int]
      val stream2 = Stream.empty[Int]
      startsWith(stream1)(stream2) should be(true)
    }

    "return false when the second stream is longer" in {
      val eval1 = Eval()
      val eval2 = Eval()
      val stream1 = eval2.stream(1, 2, 3)
      val stream2 = eval1.stream(1, 2, 3, 4, 5)
      startsWith(stream1)(stream2) should be(false)
      eval1.expect(3)
      eval2.expect(3)
    }

    "return false when the second stream differs at the head" in {
      val eval1 = Eval()
      val eval2 = Eval()
      val stream1 = eval1.stream(1, 2, 3, 4, 5)
      val stream2 = eval2.stream(3, 2, 3)
      startsWith(stream1)(stream2) should be(false)
      eval1.expect(1)
      eval2.expect(1)
    }

    "return false when the second stream differs in the tail" in {
      val eval1 = Eval()
      val eval2 = Eval()
      val stream1 = eval1.stream(1, 2, 3, 4, 5)
      val stream2 = eval2.stream(1, 2, 1)
      startsWith(stream1)(stream2) should be(false)
      eval1.expect(3)
      eval2.expect(3)
    }

    "return false when the first stream is empty" in {
      val eval = Eval()
      val stream1 = Stream.empty[Int]
      val stream2 = eval.stream(1, 2, 3)
      startsWith(stream1)(stream2) should be(false)
      eval.expect(0)
    }
  }
}