package nl.hugo.redbook.ch7

import java.util.concurrent._

import nl.hugo.redbook.ch7.ExecutorServiceDecorator._
import nl.hugo.redbook.ch7.Nonblocking.Par._
import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.time.Span
import org.scalatest.time.SpanSugar._
import org.scalatest.{ Matchers, WordSpec }

import scala.language.postfixOps

class Test7_14_Nonblocking extends WordSpec with Matchers with TimeLimitedTests {

  // Each test automatically fails after one second.
  val timeLimit: Span = 1 second

  "join" should {
    "transform a Par[Par[A]] to a Par[A]" in {
      val par: Nonblocking.Par[String] = lazyUnit("FOOBAR")
      val nestedPar: Nonblocking.Par[Nonblocking.Par[String]] = lazyUnit(par)

      val joinedPar: Nonblocking.Par[String] = join(nestedPar)

      val es: ExecutorService = Executors.newCachedThreadPool()

      es.completedTaskCount should be(0L)

      Nonblocking.Par.run(es)(joinedPar) should be("FOOBAR")

      es.completedTaskCount should be > 1L
    }
  }

  "joinViaFlatmap" should {
    "transform a Par[Par[A]] to a Par[A]" in {
      val par: Nonblocking.Par[String] = lazyUnit("FOOBAR")
      val nestedPar: Nonblocking.Par[Nonblocking.Par[String]] = lazyUnit(par)

      val joinedPar: Nonblocking.Par[String] = joinViaFlatMap(nestedPar)

      val es: ExecutorService = Executors.newCachedThreadPool()

      es.completedTaskCount should be(0L)

      Nonblocking.Par.run(es)(joinedPar) should be("FOOBAR")

      es.completedTaskCount should be > 1L
    }
  }

  "flatmapViaJoin" should {
    def candidates: Map[String, String] = Map("F" -> "First", "S" -> "Second", "T" -> "Third")

    candidates.foreach {
      case (index, item) =>
        s"select $item with $index from $candidates" in {
          val es: ExecutorService = Executors.newCachedThreadPool

          val selector: Nonblocking.Par[String] = lazyUnit(index)

          val parCandidates: Map[String, Nonblocking.Par[String]] = candidates.mapValues(lazyUnit(_))

          val candidate: Nonblocking.Par[String] = flatMapViaJoin(selector)(parCandidates)

          es.completedTaskCount should be(0L)

          Nonblocking.Par.run(es)(candidate) should be(item)

          es.completedTaskCount should be > 0L
        }
    }
  }
}
