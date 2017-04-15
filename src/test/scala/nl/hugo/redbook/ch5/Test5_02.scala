package nl.hugo.redbook.ch5

import nl.hugo.redbook.ch5.spec._

class Test5_02 extends TakeSpec with DropSpec {

  override def take[A](s: Stream[A]) = s.take
  override def drop[A](s: Stream[A]) = s.drop

  takeTest("take")
  dropTest("drop")
}