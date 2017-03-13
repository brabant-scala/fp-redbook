package nl.hugo.redbook.ch5.spec

import nl.hugo.redbook.Spec
import nl.hugo.redbook.ch5.{ Empty, Stream }
import nl.hugo.redbook.ch5.util.Eval

trait HeadOptionSpec extends Spec {

  def headOption[A](s: Stream[A]): Option[A]

  def headOptionTest(name: String) = name should {

    "return some first element for a non-empty stream" in {
      val eval = Eval()
      val stream = eval.stream(1, 2, 3)
      headOption(stream) should be(Some(1))
      eval.expect(1)
    }

    "return none for an empty stream" in {
      headOption(Empty) should be(None)
    }
  }
}