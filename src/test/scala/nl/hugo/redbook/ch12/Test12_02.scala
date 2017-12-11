package nl.hugo.redbook.ch12

import org.scalatest.{Matchers, WordSpec}

import scala.language.postfixOps

class Test12_02 extends WordSpec with Matchers {
  // In order to properly test sequence, we need to create an applicative
  val optionApplicativeWithMap2 = new Applicative[Option] {
    override def unit[A](a: => A): Option[A] = Some(a)

    override def map2[A, B, C](fa: Option[A], fb: Option[B])(f: (A, B) => C): Option[C] =
      for {
        a <- fa
        b <- fb
      } yield f(a, b)
  }

  val optionApplicativeWithApply = new Applicative[Option] {
    override def unit[A](a: => A): Option[A] = Some(a)

    override def apply[A, B](fab: Option[A => B])(fa: Option[A]): Option[B] =
      for {
        f <- fab
        a <- fa
      } yield f(a)
  }

  "An applicative with map2 implemented" should {
    "apply a some value to some function" in {
      optionApplicativeWithMap2.apply(Some((i: Int) => i.toString))(Some(1)) should be(Some("1"))
    }

    "not apply no value to some function" in {
      optionApplicativeWithMap2.apply(Some((i: Int) => i.toString))(None) should be(None)
    }

    "not apply some value to no fuction" in {
      optionApplicativeWithMap2.apply(None)(Some(1)) should be(None)
    }

    "not apply no value to no funciton" in {
      optionApplicativeWithMap2.apply(None)(None) should be(None)
    }
  }

  "An applicative with apply implemented" should {
    "map2 some value and some value" in {
      optionApplicativeWithApply.map2(Some(1), Some("a"))((a, b) => (a, b)) should be(None)
    }

    "not map2 no value and some value" in {
      optionApplicativeWithApply.map2(None, Some("a"))((a, b) => (a, b)) should be(None)
    }

    "not map2 some value and no value" in {
      optionApplicativeWithApply.map2(Some(1), None)((a, b) => (a, b)) should be(None)
    }

    "not map2 no value and no value" in {
      optionApplicativeWithApply.map2(None, None)((a, b) => (a, b)) should be(None)
    }

    "map some value" in {
      optionApplicativeWithApply.map(Some(1))(_.toString) should be(Some("1"))
    }

    "not map no value" in {
      optionApplicativeWithApply.map(None)(_.toString) should be(None)
    }
  }
}