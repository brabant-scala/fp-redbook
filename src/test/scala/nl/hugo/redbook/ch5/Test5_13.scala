package nl.hugo.redbook.ch5

import nl.hugo.redbook.ch5.spec._
import org.scalatest.{ FlatSpec, Matchers }

class Test5_13 extends FlatSpec with Matchers with TakeWhileBehavior {
  def takeWhile(s: Stream[Int])(f: Int => Boolean): Stream[Int] = s.takeWhileViaUnfold(f)

  "A takeWhileViaUnfold" should behave like aTakeWhileFunction(takeWhile)
}

class Test5_13_old extends MapSpec with TakeSpec with ZipWithSpec with ZipAllSpec {

  override def map[A, B](s: Stream[A]) = s.mapViaUnfold
  override def take[A](s: Stream[A]) = s.takeViaUnfold
  override def zipWith[A, B, C](s: Stream[A]) = s.zipWith
  override def zipAll[A, B](s: Stream[A]) = s.zipAll

  mapTest("map (via Unfold)")
  takeTest("take (via Unfold)")

  zipWithTest("zipWith")
  zipAllTest("zipAll")
}