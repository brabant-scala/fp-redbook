package nl.hugo.redbook.ch7

import java.util.concurrent._

import nl.hugo.redbook.ch7.ExecutorServiceDecorator._
import nl.hugo.redbook.ch7.Par._
import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.time.Span
import org.scalatest.time.SpanSugar._
import org.scalatest.{ Matchers, WordSpec }

import scala.language.postfixOps

class Test7_05 extends WordSpec with Matchers with TimeLimitedTests {

  // Each test automatically fails after one second.
  val timeLimit: Span = 1 second

  val es: ExecutorService = Executors.newFixedThreadPool(1)

  "Par.sequence" should {
    "transform a List[Par[A]] into Par[List[A]]" in {
      val es: ExecutorService = Executors.newFixedThreadPool(1)

      val l: List[Par[String]] = List(unit("First"), lazyUnit("Second"), unit("Third"))

      val s: Par[List[String]] = sequence(l)

      es.completedTaskCount should be(0)

      assert(Par.run(es)(Par.sequence(l)).get == List("First", "Second", "Third"))

      es.completedTaskCount should be > 0L
    }
  }
}
