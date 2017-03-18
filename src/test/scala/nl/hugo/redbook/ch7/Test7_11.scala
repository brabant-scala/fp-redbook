package nl.hugo.redbook.ch7

import java.util.concurrent._

import nl.hugo.redbook.ch7.Par._
import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.time.Span
import org.scalatest.time.SpanSugar._
import org.scalatest.{Matchers, WordSpec}

import scala.language.postfixOps

class Test7_11 extends WordSpec with Matchers with TimeLimitedTests {

  // Each test automatically fails after one second.
  val timeLimit: Span = 1 second

  val candidates: List[String] = List("First", "Second", "third")

  val lazyCandidates: List[Par[String]] = candidates.map(lazyUnit(_))

  "Par.choiceN" should {
    "select the correct element" in {
      for (index <- candidates.indices) {
        val es: ThreadPoolExecutor = Executors.newCachedThreadPool.asInstanceOf[ThreadPoolExecutor]

        val selector: Par[Int] = lazyUnit(index)

        val candidate: Par[String] = choiceN(selector)(lazyCandidates)

        es.getCompletedTaskCount should be(0)

        Par.run(es)(candidate).get should be(candidates(index))

        es.getCompletedTaskCount should be > 0L
      }
    }
  }

  "Par.choiceViaChooseN" should {
    val firstChoice = lazyUnit("First")
    val secondChoice = lazyUnit("Second")

    "select the correct element element" in {
      Map(true -> "First", false -> "Second").foreach {
        case (input, reference) =>
          val es: ThreadPoolExecutor = Executors.newCachedThreadPool.asInstanceOf[ThreadPoolExecutor]

          val selector: Par[Boolean] = lazyUnit(input)

          val candidate: Par[String] = choiceViaChoiceN(selector)(firstChoice, secondChoice)

          es.getCompletedTaskCount should be(0)

          Par.run(es)(candidate).get should be(reference)

          es.getCompletedTaskCount should be > 0L
      }
    }
  }
}