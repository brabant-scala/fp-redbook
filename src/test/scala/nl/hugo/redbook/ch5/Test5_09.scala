package nl.hugo.redbook.ch5

import nl.hugo.redbook.ch5.spec.FromSpec

class Test5_09 extends FromSpec {

  override def from = Stream.from

  fromTest("from")
}