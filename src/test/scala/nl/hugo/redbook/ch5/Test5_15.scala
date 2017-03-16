package nl.hugo.redbook.ch5

import nl.hugo.redbook.ch5.spec.TailsSpec

class Test5_15 extends TailsSpec {

  override def tails[A](s: Stream[A]) = s.tails

  tailsTest("tails")
}