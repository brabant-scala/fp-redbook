package nl.hugo.redbook.ch7

import java.util.concurrent._

import nl.hugo.redbook.ch7.ExecutorServiceDecorator._
import nl.hugo.redbook.ch7.Par._
import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.time.Span
import org.scalatest.time.SpanSugar._
import org.scalatest.{ Matchers, WordSpec }

import scala.language.postfixOps

class Test7_04 extends WordSpec with Matchers with TimeLimitedTests {

  // Each test automatically fails after one second.
  val timeLimit: Span = 1 second

  val es: ExecutorService = Executors.newFixedThreadPool(1)

  "asyncF" should {
    "evaluate a function on a separate thread" in {
      def f: Int => Int = i => i + 1

      val af: Int => Par[Int] = asyncF(f)

      val par: Par[Int] = af(10)

      es.completedTaskCount should be(0)

      Par.run(es)(par).get should be(f(10))

      es.completedTaskCount should be > 0L
    }
  }
}