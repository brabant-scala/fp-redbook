package nl.hugo.redbook.ch5.spec

import nl.hugo.redbook.Spec
import nl.hugo.redbook.ch5.Stream
import nl.hugo.redbook.ch5.util.Eval

trait ToListSpec extends Spec {

  def toList[A](s: Stream[A]): List[A]

  def toListTest(name: String) = name should {

    "turn a non-empty stream into a list" in {
      val eval = Eval()
      val stream = eval.stream(1, 2, 3)
      eval.expect(0)
      toList(stream) should be(List(1, 2, 3))
      eval.expect(3)
    }

    "turn an empty stream into Nil" in {
      val stream = Stream.empty[Boolean]
      toList(stream) should be(Nil)
    }
  }
}