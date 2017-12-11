package nl.hugo.redbook.ch12

import org.scalatest.{Matchers, WordSpec}

import scala.language.postfixOps

class Test12_03 extends WordSpec with Matchers {
  val optionApplicative = new Applicative[Option] {
    override def unit[A](a: => A): Option[A] = Some(a)

    override def apply[A, B](fab: Option[A => B])(fa: Option[A]): Option[B] =
      for {
        f <- fab
        a <- fa
      } yield f(a)
  }

  def concat(s1: String, s2: Int, s3: Long): String = s"$s1$s2$s3"

  def concat(s1: String, s2: Int, s3: Long, s4: Char): String = s"$s1$s2$s3$s4"

  "An optionApplicative" should {
    "map3 some values" in {
      optionApplicative.map3(Some("1"), Some(2), Some(3L))(concat) should be(Some("123"))
    }

    "not map3 with None values" in {
      optionApplicative.map3(None, Some(2), Some(3L))(concat) should be(None)
    }

    "not map3 with None values 2" in {
      optionApplicative.map3(Some("1"), None, Some(3L))(concat) should be(None)
    }

    "not map3 with None values 3" in {
      optionApplicative.map3(Some("1"), Some(2), None)(concat) should be(None)
    }

    "map4 some values" in {
      optionApplicative.map4(Some("1"), Some(2), Some(3L), Some('4'))(concat) should be(Some("1234"))
    }

    "not map4 with None values" in {
      optionApplicative.map4(None, Some(2), Some(3L), Some('4'))(concat) should be(None)
    }

    "not map4 with None values 2" in {
      optionApplicative.map4(Some("1"), None, Some(3L), Some('4'))(concat) should be(None)
    }

    "not map4 with None values 3" in {
      optionApplicative.map4(Some("1"), Some(2), None, Some('4'))(concat) should be(None)
    }

    "not map4 with None values 4" in {
      optionApplicative.map4(Some("1"), Some(2), Some(3L), None)(concat) should be(None)
    }

    "not map4 with None values all" in {
      optionApplicative.map4(None, None, None, None)(concat) should be(None)
    }
  }
}