package nl.hugo.redbook.ch7

import java.util.concurrent._

import nl.hugo.redbook.ch7.Nonblocking.Par._
import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.time.Span
import org.scalatest.time.SpanSugar._
import org.scalatest.{Matchers, WordSpec}

import scala.language.postfixOps

class Test7_12_Nonblocking extends WordSpec with Matchers with TimeLimitedTests {

  // Each test automatically fails after one second.
  val timeLimit: Span = 1 second

  "choiceMap" should {
    "select the correct value from a map" in {
      val candidates: Map[String, String] = Map("F" -> "First", "S" -> "Second", "T" -> "Third")

      candidates.foreach {
        case (index, item) =>
          val es: ThreadPoolExecutor = Executors.newCachedThreadPool.asInstanceOf[ThreadPoolExecutor]

          val selector: Nonblocking.Par[String] = lazyUnit(index)

          val parCandidates: Map[String, Nonblocking.Par[String]] = candidates.mapValues(lazyUnit(_))

          val candidate: Nonblocking.Par[String] = choiceMap(selector)(parCandidates)

          es.getCompletedTaskCount should be(0)

          Nonblocking.Par.run(es)(candidate) should be(item)

          es.getCompletedTaskCount should be > 0L
      }
    }
  }
}
