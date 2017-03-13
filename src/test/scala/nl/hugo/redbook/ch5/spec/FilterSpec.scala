package nl.hugo.redbook.ch5.spec

import nl.hugo.redbook.Spec
import nl.hugo.redbook.ch5.{ Empty, Stream }
import nl.hugo.redbook.ch5.util.Eval

trait FilterSpec extends Spec {

  def filter[A](s: Stream[A]): (A => Boolean) => Stream[A]

  def filterTest[A](name: String) = name should {

    "return all elements that satisfy the predicate" in {
      val eval = Eval()
      val stream = eval.stream(1, 2, 3, 2, 1)
      val result = filter(stream)(_ > 1)
      eval.expect(2) // first 2 values must be evaluated to decide on head
      result.toList should be(List(2, 3, 2))
      eval.expect(5)
    }

    "return an empty stream for an empty stream" in {
      val stream = Stream.empty[Int]
      filter(stream)(_ == 2) should be(Empty)
    }
  }
}