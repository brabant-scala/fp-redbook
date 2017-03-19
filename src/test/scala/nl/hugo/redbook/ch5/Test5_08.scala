package nl.hugo.redbook.ch5

import nl.hugo.redbook.ch5.spec.ConstantSpec

class Test5_08 extends ConstantSpec {

  override def constant[A] = Stream.constant

  constantTest("constant")
}