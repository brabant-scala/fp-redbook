package nl.hugo.redbook.ch5

import nl.hugo.redbook.ch5.spec._

class Test5_12 extends FibsSpec with FromSpec with ConstantSpec {

  override def fibs = Stream.fibsViaUnfold
  override def from = Stream.fromViaUnfold
  override def constant[A] = Stream.constantViaUnfold

  fibsTest("fibs (via unfold)")
  fromTest("from (via unfold)")
  constantTest("constant (via unfold)")

  "ones" should {

    "return an infinite stream with the same constant" in {
      Stream.onesViaUnfold.take(7).toList should be(List(1, 1, 1, 1, 1, 1, 1))
    }
  }
}