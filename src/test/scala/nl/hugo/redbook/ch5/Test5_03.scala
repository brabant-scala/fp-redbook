package nl.hugo.redbook.ch5

import nl.hugo.redbook.ch5.spec.TakeWhileSpec

class Test5_03 extends TakeWhileSpec {

  override def takeWhile[A](s: Stream[A]) = s.takeWhile

  takeWhileTest("takeWhile")
}