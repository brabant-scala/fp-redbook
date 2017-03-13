package nl.hugo.redbook.ch5

import nl.hugo.redbook.ch5.spec.ScanRightSpec

class Test5_16 extends ScanRightSpec {

  override def scanRight[A, B](s: Stream[A]) = s.scanRight

  scanRightTest("scanRight")
}