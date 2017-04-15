package nl.hugo.redbook.ch5.spec

import nl.hugo.redbook.Spec
import nl.hugo.redbook.ch5.Stream

trait FibsSpec extends Spec {

  def fibs: Stream[Int]

  def fibsTest(name: String) = name should {

    "return a stream with the fibonacci numbers" in {
      fibs.take(8).toList should be(List(0, 1, 1, 2, 3, 5, 8, 13))
    }
  }
}