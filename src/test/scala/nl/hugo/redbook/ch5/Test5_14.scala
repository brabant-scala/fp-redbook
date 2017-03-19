package nl.hugo.redbook.ch5

import nl.hugo.redbook.ch5.spec.StartsWithSpec

class Test5_14 extends StartsWithSpec {

  override def startsWith[A](s: Stream[A]) = s.startsWith

  startsWithTest("startsWith")
}