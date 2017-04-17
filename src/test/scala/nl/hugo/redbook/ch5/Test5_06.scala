package nl.hugo.redbook.ch5

import nl.hugo.redbook.ch5.spec.HeadOptionSpec

class Test5_06 extends HeadOptionSpec {

  override def headOption[A](s: Stream[A]) = s.headOption

  headOptionTest("headOption")
}