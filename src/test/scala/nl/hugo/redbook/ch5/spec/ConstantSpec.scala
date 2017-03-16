package nl.hugo.redbook.ch5.spec

import nl.hugo.redbook.Spec
import nl.hugo.redbook.ch5.Stream

trait ConstantSpec extends Spec {

  def constant[A]: A => Stream[A]

  def constantTest(name: String) = name should {

    "return an infinite stream with the same constant" in {
      constant(42).take(3).toList should be(List(42, 42, 42))
    }
  }
}