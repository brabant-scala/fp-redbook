package nl.hugo.redbook.ch5.spec

import nl.hugo.redbook.Spec
import nl.hugo.redbook.ch5.Stream
import nl.hugo.redbook.ch5.util.Eval

trait ForAllSpec extends Spec {

  def forAll[A](s: Stream[A]): (A => Boolean) => Boolean

  def forAllTest(name: String) = name should {

    "return true when all elements satisfy the predicate" in {
      val eval = Eval()
      val stream = eval.stream(1, 2, 3)
      forAll(stream)(_ != 0) should be(true)
      eval.expect(3)
    }

    "return false when the first element does not satisfy the predicate" in {
      val eval = Eval()
      val stream = eval.stream(1, 2, 3)
      forAll(stream)(_ > 1) should be(false)
      eval.expect(1)
    }

    "return false when the last element does not satisfy the predicate" in {
      val eval = Eval()
      val stream = eval.stream(1, 2, 3)
      forAll(stream)(_ < 3) should be(false)
      eval.expect(3)
    }

    "return false when some element does not satisfy the predicate" in {
      val eval = Eval()
      val stream = eval.stream(1, 2, 3)
      forAll(stream)(_ != 2) should be(false)
      eval.expect(2)
    }

    "return true for an empty stream" in {
      val stream = Stream.empty[Boolean]
      forAll(stream)(identity) should be(true)
    }
  }
}