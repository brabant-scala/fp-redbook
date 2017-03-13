package nl.hugo.redbook.ch5

import nl.hugo.redbook.ch5.spec.FibsSpec

class Test5_10 extends FibsSpec {

  override def fibs = Stream.fibs

  fibsTest("fibs")
}