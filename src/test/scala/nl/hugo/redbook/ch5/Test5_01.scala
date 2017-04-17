package nl.hugo.redbook.ch5

import nl.hugo.redbook.ch5.spec.ToListSpec

class Test5_01 extends ToListSpec {

  override def toList[A](s: Stream[A]) = s.toList

  toListTest("toList")
}