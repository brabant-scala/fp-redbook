package nl.hugo.redbook.ch7

import java.util.concurrent._

import nl.hugo.redbook.ch7.ExecutorServiceDecorator._
import nl.hugo.redbook.ch7.Par._
import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.time.Span
import org.scalatest.time.SpanSugar._
import org.scalatest.{ Matchers, WordSpec }

import scala.language.postfixOps

class Test7_13 extends WordSpec with Matchers with TimeLimitedTests {

  // Each test automatically fails after one second.
  val timeLimit: Span = 1 second

  "chooser" should {
    def candidates: Map[String, String] = Map("F" -> "First", "S" -> "Second", "T" -> "Third")

    candidates.foreach {
      case (index, item) =>
        s"select $item with $index from $candidates" in {
          val es: ExecutorService = Executors.newCachedThreadPool

          val selector: Par[String] = lazyUnit(index)

          val parCandidates: Map[String, Par[String]] = candidates.mapValues(lazyUnit(_))

          val candidate: Par[String] = chooser(selector)(parCandidates)

          es.completedTaskCount should be(0L)

          Par.run(es)(candidate).get should be(item)

          es.completedTaskCount should be > 0L
        }
    }
  }

  "choiceViaChooser" should {
    val firstChoice = lazyUnit("First")
    val secondChoice = lazyUnit("Second")

    Map(true -> "First", false -> "Second").foreach {
      case (input, reference) =>
        s"select the $reference item using $input" in {
          val es: ExecutorService = Executors.newCachedThreadPool

          val selector: Par[Boolean] = lazyUnit(input)

          val candidate: Par[String] = choiceViaChooser(selector)(firstChoice, secondChoice)

          es.completedTaskCount should be(0)

          Par.run(es)(candidate).get should be(reference)

          es.completedTaskCount should be > 0L
        }
    }
  }

  "choiceNViaChooser" should {
    val candidates: List[String] = List("First", "Second", "third")

    val lazyCandidates: List[Par[String]] = candidates.map(lazyUnit(_))

    for (index <- candidates.indices) {
      s"selects item $index from $candidates" in {
        val es: ExecutorService = Executors.newCachedThreadPool

        val selector: Par[Int] = lazyUnit(index)

        val candidate: Par[String] = choiceNViaChooser(selector)(lazyCandidates)

        es.completedTaskCount should be(0)

        Par.run(es)(candidate).get should be(candidates(index))

        es.completedTaskCount should be > 0L
      }
    }
  }
}
