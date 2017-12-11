package nl.hugo.redbook.ch12

import org.scalatest.{Matchers, WordSpec}

import scala.language.postfixOps

class Test12_11 extends WordSpec with Matchers {
  val optionApplicative = new Applicative[Option] {
    override def unit[A](a: => A): Option[A] = Some(a)

    override def apply[A, B](fab: Option[A => B])(fa: Option[A]): Option[B] =
      for {
        f <- fab
        a <- fa
      } yield f(a)
  }

  "An applicative product" should {
    "sequenceMap some values" in {
      optionApplicative.sequenceMap(Map("foo" -> Some("bar"), "foz" -> Some("baz"))) should
        be(Some(Map("foo" -> "bar", "foz" -> "baz")))
    }

    "not sequenceMap if some values are missing" in {
      optionApplicative.sequenceMap(Map("foo" -> None, "foz" -> Some("baz"))) should
        be(None)
    }

    "sequenceMap an empty map" in {
      optionApplicative.sequenceMap(Map()) should
        be(Some(Map()))
    }
  }
}