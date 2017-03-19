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

  val es: ExecutorService = Executors.newCachedThreadPool

  "Par.parFilter" should {
    "remove items" in {
      val l: List[Int] = List(0, 1, 2, 3, 4, 5)

      def isEven(i: Int): Boolean = i % 2 == 0

      val fl: Par[List[Int]] = parFilter(l)(isEven)

      es.completedTaskCount should be(0)

      Par.run(es)(fl).get should be(List(0, 2, 4))

      es.completedTaskCount should be > 0L
    }
  }
}
