package nl.hugo.redbook.ch10

import java.util.concurrent._

import nl.hugo.redbook.ch10.Monoid._
import nl.hugo.redbook.ch7.ExecutorServiceDecorator._
import nl.hugo.redbook.ch7.Nonblocking._
import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.time.Span
import org.scalatest.time.SpanSugar._
import org.scalatest.{ Matchers, WordSpec }

import scala.language.postfixOps

class Test10_08 extends WordSpec with Matchers with TimeLimitedTests {

  // Each test automatically fails after one second.
  val timeLimit: Span = 1 second

  "Monoid.par" should {
    "parallelize a monoid" in {
      val es: ExecutorService = Executors.newFixedThreadPool(1)

      val a1 = Par.unit(1)
      val a2 = Par.unit(20)

      val parMonoid = par(intAddition).op(a1, a2)

      es.completedTaskCount should be(0)

      Par.run(es)(parMonoid) should be(21)

      es.completedTaskCount should be >= 1L
    }
  }

  "Monoid.parFoldMap" should {
    val stringMonoid = new Monoid[String] {
      def op(a1: String, a2: String): String = a1 + a2

      def zero = ""
    }

    def pfm(seq: IndexedSeq[Char]): Par[String] = parFoldMap(seq, stringMonoid)(_.toString)

    "correctly concatenate an empty list of chars" in {
      val es: ExecutorService = Executors.newFixedThreadPool(1)

      val l = IndexedSeq.empty[Char]

      Par.run(es)(pfm(l)) shouldBe ""
    }

    "correctly concatenate a list of chars into a string" in {
      val es: ExecutorService = Executors.newFixedThreadPool(1)

      val l = IndexedSeq(
        'a', 'b', 'c', 'd', 'e', 'f',
        'g', 'h', 'i', 'j', 'k', 'l',
        'm', 'n', 'o', 'p', 'q', 'r',
        's', 't', 'u', 'v', 'w', 'x',
        'y', 'z'
      )

      es.completedTaskCount should be(0)

      Par.run(es)(pfm(l)) shouldBe "abcdefghijklmnopqrstuvwxyz"

      es.completedTaskCount should be >= 1L
    }
  }
}