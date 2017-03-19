package nl.hugo.redbook.ch7

import java.util.concurrent._

import nl.hugo.redbook.ch7.Par._
import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.time.Span
import org.scalatest.time.SpanSugar._
import org.scalatest.{ Matchers, WordSpec }

import scala.language.postfixOps

class Test7_03 extends WordSpec with Matchers with TimeLimitedTests {

  // Each test automatically fails after one second.
  val timeLimit: Span = 1 second

  val es: ExecutorService = Executors.newSingleThreadExecutor

  def longRunningPar[A](v: A): Par[A] = {
    es =>
      val task = new Callable[A] {
        def call(): A = {
          TimeUnit.SECONDS.sleep(100)
          v
        }
      }
      es.submit(task)
  }

  "A result from Par.map2WhileRespectingContracts" should {
    val combinedPar = map2WhileRespectingContracts(longRunningPar("FOO"), longRunningPar("BAR"))(_ + _)

    "timeout on a get" in {
      val combinedFuture = Par.run(es)(combinedPar)

      combinedFuture.isDone should be(false)
      combinedFuture.isCancelled should be(false)

      intercept[TimeoutException] {
        combinedFuture.get(10, TimeUnit.MILLISECONDS)
      }
      combinedFuture.isDone should be(false)
      combinedFuture.isCancelled should be(false)
    }

    "interrupt the computation on cancel" in {
      val combinedFuture = Par.run(es)(combinedPar)

      combinedFuture.isDone should be(false)
      combinedFuture.isCancelled should be(false)

      combinedFuture.cancel(true) should be(true)

      combinedFuture.isDone should be(false)
      combinedFuture.isCancelled should be(true)
    }
  }
}
