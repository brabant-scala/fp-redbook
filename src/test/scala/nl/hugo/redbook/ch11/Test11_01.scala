package nl.hugo.redbook.ch11

import java.util.concurrent.{ExecutorService, Executors}

import nl.hugo.redbook.ch4.{None, Option, Some}
import nl.hugo.redbook.ch5.Stream
import nl.hugo.redbook.ch7.Par
import nl.hugo.redbook.ch7.Par.Par
import nl.hugo.redbook.ch9.LocationParser
import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.time.Span
import org.scalatest.time.SpanSugar._
import org.scalatest.{Matchers, WordSpec}

import scala.language.postfixOps

import scala.{ Either => _, Option => _, Some => _, Stream => _ }

class Test11_01 extends WordSpec with Matchers with TimeLimitedTests {

  val es: ExecutorService = Executors.newCachedThreadPool

  // Each test automatically fails after one second.
  val timeLimit: Span = 1 second

  val v = "TheQuickBrownFoxJumpedOverTheLazyDog"

  def f(i: Int): String = s"i = $i"

  "Monad.parMonad" should {
    "assign a unit value" in {
      val u = Monad.parMonad.unit(v)

      Par.run(es)(u).get should be(v)
    }

    "flatMap a function" in {
      val pf: Int => Par[String] = Par.asyncF(f)
      val pv: Par[Int] = Monad.parMonad.unit(1)

      val pfv = Monad.parMonad.flatMap(pv)(pf)

      Par.run(es)(pfv).get should be("i = 1")
    }
  }

  "Monad.parserMonad" should {
    val parser = LocationParser.Impl
    val parserMonad = Monad.parserMonad(parser)
    "assign a unit value" in {
      val u = parserMonad.unit("1000")

      parser.run(u)("1000") should be(Right("1000"))
    }

    "flatMap a function" in {
      val u = parserMonad.unit("1000")

      def f(s: String) = parserMonad.unit(s"${s}0")

      parser.run(parserMonad.flatMap(u)(f))("1") should be(Right("10000"))
    }
  }

  "Monad.OptionMonad" should {
    "assign a unit value" in {
      Monad.optionMonad.unit(10) shouldBe Some(10)
    }

    "flatMap a function" in {
      val u = Monad.optionMonad.unit(10)
      val v = Monad.optionMonad.unit(9)

      def f(i: Int): Option[Int] = i match {
        case x if x < 10 => Some(x)
        case _ => None
      }

      Monad.optionMonad.flatMap(v)(f) should be(Some(9))
      Monad.optionMonad.flatMap(u)(f) should be(None)
    }
  }

  "Monad.StreamMonad" should {
    "assign a unit value" in {
      Monad.streamMonad.unit("foo").toList should be(List("foo"))
    }

    "flatMap a function" in {
      val u = Monad.streamMonad.unit(1)
      def d(i: Int):Stream[Int] = Stream(i,i)

      Monad.streamMonad.flatMap(u)(d).toList should be(List(1,1))
    }
  }

  "Monad.ListMonad" should {
    "assign a unit value" in {
      Monad.listMonad.unit("foo") should be(List("foo"))
    }

    "flatMap a function" in {
      val u = Monad.listMonad.unit(1)
      def d(i: Int):List[Int] = List(i,i)

      Monad.listMonad.flatMap(u)(d) should be(List(1,1))
    }
  }
}