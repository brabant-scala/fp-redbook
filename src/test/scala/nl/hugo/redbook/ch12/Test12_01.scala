package nl.hugo.redbook.ch12

import org.scalatest.{Matchers, WordSpec}

import scala.language.postfixOps

class Test12_01 extends WordSpec with Matchers {
  val optionApplicative = new Applicative[Option] {
    override def unit[A](a: => A): Option[A] = Some(a)

    override def map2[A, B, C](fa: Option[A], fb: Option[B])(f: (A, B) => C): Option[C] =
      for {
        a <- fa
        b <- fb
      } yield f(a, b)
  }

  "An applicative sequence" should {
    "turn a List[Some] into Some(List)" in {
      optionApplicative.sequence(
        List(
          Some(1),
          Some(2),
          Some(3),
          Some(4)
        )
      ) should be(Some(List(1, 2, 3, 4)))
    }

    "sequence a list with a None into None" in {
      optionApplicative.sequence(
        List(
          Some(1),
          Some(2),
          None,
          Some(3)
        )
      ) should be(None)
    }
  }

  "An applicative replicateM" should {

    "replicate Some(value)" in {
      optionApplicative.replicateM(4, Some(1)) should be(Some(List(1, 1, 1, 1)))
    }

    "not replicate a None" in {
      optionApplicative.replicateM(4, None) should be(None)
    }
  }

  "An applicative traverse" should {
    "traverse a list" in {
      optionApplicative.traverse(List(1, 2, 3, 4))(v => Some(v.toString)) should be(
        Some(List("1", "2", "3", "4"))
      )
    }

    "traverse a list until a None is encountered" in {
      optionApplicative.traverse(List(1, 2, 3, 4)) {
        case v if v != 3 => Some(v)
        case _ => None
      } should be(Some(List(1, 2)))
    }
  }

  "An applicative product" should {
    "pair two Somes" in {
      optionApplicative.product(Some(1), Some("a")) should be(Some(1,"a"))
    }

    "not pair when left is None" in {
      optionApplicative.product(None, Some("a")) should be(None)
    }

    "not pair when right is None" in {
      optionApplicative.product(Some(1), None) should be(None)
    }

    "not pair when both are None" in {
      optionApplicative.product(Some(1), Some("a")) should be(None)
    }
  }
}