package nl.hugo.redbook.ch5.spec

import nl.hugo.redbook.Spec
import nl.hugo.redbook.ch5.{ Empty, Stream }
import nl.hugo.redbook.ch5.util.Eval

trait TailsSpec extends Spec {

  def tails[A](s: Stream[A]): Stream[Stream[A]]

  def tailsTest(name: String) = name should {

    "return all tails for a non-empty stream" in {
      val eval = Eval()
      val stream = eval.stream(1, 2, 3, 4, 5)
      val result = tails(stream)
      eval.expect(0)
      val tailsList = result.toList
      eval.expect(0)
      tailsList.map(_.toList) should be(List(
        List(1, 2, 3, 4, 5),
        List(2, 3, 4, 5),
        List(3, 4, 5),
        List(4, 5),
        List(5),
        Nil
      ))
      eval.expect(5)
    }

    "return empty for an empty stream" in {
      val result = tails(Empty)
      val tailsList = result.toList
      tailsList.map(_.toList) should be(List(Nil))
    }
  }
}