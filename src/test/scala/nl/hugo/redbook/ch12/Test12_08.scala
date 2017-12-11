package nl.hugo.redbook.ch12

import org.scalatest.{Matchers, WordSpec}

import scala.language.postfixOps

class Test12_08 extends WordSpec with Matchers {
  val optionApplicative = new Applicative[Option] {
    override def unit[A](a: => A): Option[A] = Some(a)

    override def apply[A, B](fab: Option[A => B])(fa: Option[A]): Option[B] =
      for {
        f <- fab
        a <- fa
      } yield f(a)
  }

  val listApplicative = new Applicative[List] {
    override def unit[A](a: => A): List[A] = List(a)

    override def apply[A, B](fab: List[A => B])(fa: List[A]): List[B] =
      for {
        f <- fab
        l <- fa
      } yield f(l)
  }

  val applicativeProduct = optionApplicative.product(listApplicative)

  "An applicative product" should {
    "assign a value to a unit" in {
      applicativeProduct.unit(1) should be((Some(1), List(1)))
    }

    "map2" in {
      applicativeProduct.map2((Some(1), List(1, 2, 3)), (Some(2), List(4, 5, 6)))((_, _)) should
        be(
          (
            Some((1, 2)),
            List((1, 4), (1, 5), (1, 6), (2, 4), (2, 5), (2, 6), (3, 4), (3, 5), (3, 6))
          )
        )
    }

    "apply nothing" in {
      applicativeProduct.apply((None, List()))((None, List())) should be((None, List()))
    }

    "apply left" in {
      applicativeProduct.apply((Some((i: Int) => i.toString), List()))((Some(1), List())) should be((Some("1"), List()))
    }

    "apply right" in {
      applicativeProduct.apply(
        (Some((i: Int) => i.toString), List((i: Int) => i.toString, (i: Int) => (i + 1).toString)))((None, List(2))
      ) should
        be((None, List("2", "3")))
    }

    "apply both" in {
      applicativeProduct.apply(
        (Some((i: Int) => i.toString), List((i: Int) => i.toString, (i: Int) => (i + 1).toString)))((Some(1), List(2))
      ) should
        be((Some("1"), List("2", "3")))
    }

  }
}