package nl.hugo.redbook.ch5

import nl.hugo.redbook.ch5.spec.UnfoldSpec

class Test5_11 extends UnfoldSpec {

  override def unfold[A, S] = Stream.unfold

  unfoldTest("unfold")
}