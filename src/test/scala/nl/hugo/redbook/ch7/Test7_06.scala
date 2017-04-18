package nl.hugo.redbook.ch7

import java.util.concurrent._

import nl.hugo.redbook.ch7.ExecutorServiceDecorator._
import nl.hugo.redbook.ch7.Par._
import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.time.Span
import org.scalatest.time.SpanSugar._
import org.scalatest.{ Matchers, WordSpec }

import scala.language.postfixOps

class Test7_06 extends WordSpec with Matchers with TimeLimitedTests {

  // Each test automatically fails after one second.
  val timeLimit: Span = 1 second

  "Par.parFilter" should {
    "remove items" in {
      val es: ExecutorService = Executors.newCachedThreadPool

      val l: List[Int] = List(0, 1, 2, 3, 4, 5)

      def isEven(i: Int): Boolean = i % 2 == 0

      val fl: Par[List[Int]] = parFilter(l)(isEven)

      es.completedTaskCount should be(0)

      Par.run(es)(fl).get should be(List(0, 2, 4))

      es.completedTaskCount should be > 0L
    }
  }

  "Par.sum" should {
    "add items" in {
      val es: ExecutorService = Executors.newCachedThreadPool

      val l: IndexedSeq[Int] = Vector(0, 1, 2, 3, 4, 5)

      val fl: Par[Int] = parSum2(l)

      es.completedTaskCount should be(0)

      Par.run(es)(fl).get should be(15)

      es.completedTaskCount should be > 0L
    }
  }

  "Par.max" should {
    "return largest value" in {
      val es: ExecutorService = Executors.newCachedThreadPool

      val l: IndexedSeq[Int] = Vector(0, 1, 20, 3, 4, 5)

      val fl: Par[Int] = parMax(l)

      es.completedTaskCount should be(0)

      Par.run(es)(fl).get should be(20)

      es.completedTaskCount should be > 0L
    }
  }

  "Par.countWords" should {
    "return the total number of words in a string list" in {
      val es: ExecutorService = Executors.newCachedThreadPool

      val l: List[String] = List(
        "John goes out playing",
        "and Pete went swimming with his brother",
        "Claire was knitting a sweater with lots of wool",
        "Steve was leaving an hour ago to play football with his team",
        "Anna was reading a book about holiday destinations",
        "and grandma was looking out of the window"
      )

      val fl: Par[Int] = countWords2(l)

      es.completedTaskCount should be(0)

      Par.run(es)(fl).get should be(48)

      es.completedTaskCount should be > 0L
    }
  }
}
