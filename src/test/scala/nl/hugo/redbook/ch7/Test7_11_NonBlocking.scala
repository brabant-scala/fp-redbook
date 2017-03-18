package nl.hugo.redbook.ch7

import java.util.concurrent._

import nl.hugo.redbook.ch7.Nonblocking.Par._
import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.time.Span
import org.scalatest.time.SpanSugar._
import org.scalatest.{Matchers, WordSpec}

import scala.language.postfixOps

class Test7_11_NonBlocking extends WordSpec with Matchers with TimeLimitedTests {

  // Each test automatically fails after one second.
  val timeLimit: Span = 1 second

  "Par.choiceN" should {
    "select the correct element" in {
      val candidates: List[String] = List("First", "Second", "third")
      val lazyCandidates: List[Nonblocking.Par[String]] = candidates.map(lazyUnit(_))

      for (index <- candidates.indices) {
        val es: ThreadPoolExecutor = Executors.newFixedThreadPool(1).asInstanceOf[ThreadPoolExecutor]

        val selector: Nonblocking.Par[Int] = lazyUnit(index)

        val candidate: Nonblocking.Par[String] = choiceN(selector)(lazyCandidates)

        es.getCompletedTaskCount should be(0)

        Nonblocking.Par.run(es)(candidate) should be(candidates(index))

        es.getCompletedTaskCount should be > 0L
      }
    }
  }

  "Par.choiceViaChooseN" should {
    "select the correct element element" in {
      val firstChoice = lazyUnit("First")
      val secondChoice = lazyUnit("Second")

      Map( true -> "First", false -> "Second").foreach{
        case (input, reference) =>
          val es: ThreadPoolExecutor = Executors.newFixedThreadPool(1).asInstanceOf[ThreadPoolExecutor]

          val selector = lazyUnit(input)

          val candidate: Nonblocking.Par[String] = choiceViaChoiceN(selector)(firstChoice, secondChoice)

          es.getCompletedTaskCount should be(0)

          Nonblocking.Par.run(es)(candidate) should be(reference)

          es.getCompletedTaskCount should be > 0L
      }
    }
  }
}