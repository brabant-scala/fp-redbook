package nl.hugo.redbook.ch8

import org.scalacheck.Properties
import org.scalacheck.{ Gen => SCGen }
import org.scalacheck.Prop.forAll
import org.scalatest.{ Matchers, WordSpec }

object SumSpecification extends Properties("Sum") {
  def sum(l: List[Int]): Int = l.foldRight(0)(_ + _)

  property("sum") = forAll { l: List[Int] => sum(l) == l.sum }

  property("list of same elements") = forAll(SCGen.choose(0, 100), SCGen.choose(-1000, 1000)) { (n: Int, v: Int) => sum(List.fill(n)(v)) == n * v }

  property("communative") = forAll { l: List[Int] => sum(l) == sum(l.reverse) }

  property("associative") = forAll { (l: List[Int], r: List[Int]) => sum(l) + sum(r) == sum(l ++ r) }
}
