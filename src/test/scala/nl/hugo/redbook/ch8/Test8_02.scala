package nl.hugo.redbook.ch8

import org.scalacheck.Properties
import org.scalacheck.{ Gen => SCGen }
import org.scalacheck.Prop.forAll
import org.scalatest.{ Matchers, WordSpec }

object MaxSpecification extends Properties("Sum") {
  def max(l: List[Int]): Int = l.foldRight(Int.MinValue)(_ max _)

  property("max") = forAll { l: List[Int] => max(l) == l.max }

  property("communative") = forAll { l: List[Int] => max(l) == max(l.reverse) }

  property("associative") = forAll { (l: List[Int], r: List[Int]) => max(l).max(max(r)) == max(l ++ r) }
}
