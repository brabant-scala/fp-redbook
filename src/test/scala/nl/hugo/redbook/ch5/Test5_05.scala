package nl.hugo.redbook.ch5

import nl.hugo.redbook.ch5.spec.TakeWhileBehavior
import org.scalatest.Matchers
import org.scalatest.FlatSpec

class Test5_05 extends FlatSpec with Matchers with TakeWhileBehavior {
  def takeWhile(s: Stream[Int])(f: Int => Boolean): Stream[Int] = s.takeWhileViaFoldRight(f)

  "A takeWhileViaFoldRight" should behave like aTakeWhileFunction(takeWhile)
}