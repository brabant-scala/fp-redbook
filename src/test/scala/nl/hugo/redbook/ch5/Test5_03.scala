package nl.hugo.redbook.ch5

import nl.hugo.redbook.ch5.spec.TakeWhileBehavior
import org.scalatest.{ FlatSpec, Matchers }

class Test5_03 extends FlatSpec with Matchers with TakeWhileBehavior {
  def takeWhile(s: Stream[Int])(f: Int => Boolean): Stream[Int] = s.takeWhile(f)

  "A takeWhile function" should behave like aTakeWhileFunction(takeWhile)
}