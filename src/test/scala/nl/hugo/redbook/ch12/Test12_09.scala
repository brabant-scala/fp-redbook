package nl.hugo.redbook.ch12

import org.scalatest.{Matchers, WordSpec}

import scala.language.postfixOps

class Test12_09 extends WordSpec with Matchers {
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

  val applicativeComposition = optionApplicative.compose(listApplicative)

  "An applicative product" should {
    "assign a value to a unit" in {
      applicativeComposition.unit(1) should be(Some(List(1)))
    }

    "map2" in {
      applicativeComposition.map2(Some(List(1, 2, 3)), Some(List(4, 5, 6)))((_, _)) should
        be(
          Some(List((1, 4), (1, 5), (1, 6), (2, 4), (2, 5), (2, 6), (3, 4), (3, 5), (3, 6)))
        )
    }

    "apply nothing" in {
      applicativeComposition.apply(None)(None) should be(None)
    }

    "apply nothing 2" in {
      applicativeComposition.apply(Some(List()))(Some(List())) should be(Some(List()))
    }

    "apply right" in {
      applicativeComposition.apply(Some(List((i: Int) => i.toString, (i: Int) => (i + 1).toString)))(Some(List(2, 30))) should
        be(Some(List("2", "30", "3", "31")))
    }
  }
}