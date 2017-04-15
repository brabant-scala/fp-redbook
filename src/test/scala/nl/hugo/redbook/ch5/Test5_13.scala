package nl.hugo.redbook.ch5

import nl.hugo.redbook.ch5.spec._

class Test5_13 extends MapSpec with TakeSpec with TakeWhileSpec with ZipWithSpec with ZipAllSpec {

  override def map[A, B](s: Stream[A]) = s.mapViaUnfold
  override def take[A](s: Stream[A]) = s.takeViaUnfold
  override def takeWhile[A](s: Stream[A]) = s.takeWhileViaUnfold
  override def zipWith[A, B, C](s: Stream[A]) = s.zipWith
  override def zipAll[A, B](s: Stream[A]) = s.zipAll

  mapTest("map (via Unfold)")
  takeTest("take (via Unfold)")
  takeWhileTest("takeWhile (via Unfold)")
  zipWithTest("zipWith")
  zipAllTest("zipAll")
}