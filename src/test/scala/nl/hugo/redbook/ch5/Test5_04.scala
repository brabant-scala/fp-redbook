package nl.hugo.redbook.ch5

import nl.hugo.redbook.ch5.spec.ForAllSpec

class Test5_04 extends ForAllSpec {

  override def forAll[A](s: Stream[A]) = s.forAll

  forAllTest("forAll")
}