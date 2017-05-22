package nl.hugo.redbook.ch8

import nl.hugo.redbook.ch6.RNG
import org.scalatest.{Matchers, WordSpec}
import Prop._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class Test8_14 extends WordSpec with Matchers {
  "Gen.sortedProp" should {
    def runTestWithOriginalProperty(sortingFunction: List[Int] => List[Int]) = {
      val rng = RNG.Simple(System.nanoTime())

      // Exercise 8.14
      // val sortedProp: Prop = ???
      val smallInt = Gen.choose(-10,10)
      val sortedProp = Prop.forAll(Gen.listOf(smallInt)) { ns =>
        val nss = sortingFunction(ns)
        // We specify that every sorted list is either empty, has one element,
        // or has no two consecutive elements `(a,b)` such that `a` is greater than `b`.
        nss.isEmpty || nss.tail.isEmpty || !nss.zip(nss.tail).exists {
          case (a,b) => a > b
        } &&
          // Also, the sorted list should have all the elements of the input list,
          !ns.exists(!nss.contains(_)) &&
          // and it should have no elements not in the input list.
          !nss.exists(!ns.contains(_))
      }

      sortedProp.run(100, 1000, rng)

    }

    def runTestWith(sortingFunction: List[Int] => List[Int]) = {
      val rng = RNG.Simple(System.nanoTime())

      def equalMaps(x: Map[Int, Int], y: Map[Int, Int]): Boolean = {
        x.foldLeft(true){
          case (acc, (k, v)) => acc && y.getOrElse(k, v + 1)==v
        } &&
        y.foldLeft(true){
          case (acc, (k, v)) => acc && x.getOrElse(k, v + 1)==v
        }
      }

      // Exercise 8.14
      // val sortedProp: Prop = ???
      val smallInt = Gen.choose(-10,10)
      val sortedProp = Prop.forAll(Gen.listOf(smallInt)) { ns =>
        val nss = sortingFunction(ns)
        // We specify that every sorted list is either empty, has one element,
        // or has no two consecutive elements `(a,b)` such that `a` is greater than `b`.
        nss.isEmpty || !nss.zip(nss.tail).exists {
          case (a,b) => a > b
        } &&
        // both lists should contain exactly the same elements with the same arities
        equalMaps(
          ns.groupBy(x => x).mapValues(_.length),
          nss.groupBy(x => x).mapValues(_.length)
        )
      }

      sortedProp.run(100, 1000, rng)
    }

    "should pass" in {
      runTestWithOriginalProperty(_.sorted) should be(Prop.Passed)

    }

    "does not work" in {
      val buffer: ArrayBuffer[(List[Int], List[Int])] = new ArrayBuffer

      runTestWithOriginalProperty(list => {
        var map = list.groupBy(x => x)
        val multiples = map.filter(_._2.length >= 2)
        if (multiples.size >= 2) {
          val i = multiples.iterator
          val (key1, list1) = i.next()
          val (key2, list2) = i.next()
          map = map.updated(key1, list1.drop(1)).updated(key2, key2::list2)
        }
        val values = map.values.flatten.toList.sorted
        buffer += ((list, values))
        values
      }) should be(Passed)

      buffer.foreach({
        case (input, output) => input.sorted should equal(output.sorted)
      })
    }

    "correct version" in {
      runTestWith(_.sorted) should be (Passed)

    }
    "now it works" in {
      val buffer: ArrayBuffer[(List[Int], List[Int])] = new ArrayBuffer

      val result = runTestWith(list => {
        var map = list.groupBy(x => x)
        val multiples = map.filter(_._2.length >= 2)
        if (multiples.size >= 2) {
          val i = multiples.iterator
          val (key1, list1) = i.next()
          val (key2, list2) = i.next()
          map = map.updated(key1, list1.drop(1)).updated(key2, key2::list2)
        }
        val values = map.values.flatten.toList.sorted
        buffer += ((list, values))
        values
      })

      result.isFalsified should be (true)
    }
  }
}
