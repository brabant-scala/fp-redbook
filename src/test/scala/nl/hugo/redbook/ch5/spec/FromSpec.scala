package nl.hugo.redbook.ch5.spec

import nl.hugo.redbook.Spec
import nl.hugo.redbook.ch5.Stream

trait FromSpec extends Spec {

  def from: Int => Stream[Int]

  def fromTest(name: String) = name should {

    "return a stream that counts upwards from a number" in {
      from(3).take(5).toList should be(List(3, 4, 5, 6, 7))
    }
  }
}