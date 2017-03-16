package nl.hugo.redbook.ch5

import nl.hugo.redbook.ch3.List
import nl.hugo.redbook.ch5.spec._

class Test5_07 extends MapSpec with FilterSpec with AppendSpec with FlatMapSpec {

  override def map[A, B](s: Stream[A]) = s.map
  override def filter[A](s: Stream[A]) = s.filter
  override def append[A, B >: A](s: Stream[A]) = s.append
  override def flatMap[A, B](s: Stream[A]) = s.flatMap

  mapTest("map (via foldRight)")
  filterTest("filter (via foldRight)")
  appendTest("append (via foldRight)")
  flatMapTest("flatMap (via foldRight)")
}