package nl.hugo.redbook.ch12

import org.scalatest.{Matchers, WordSpec}

import scala.language.postfixOps

class Test12_04 extends WordSpec with Matchers {
  "A streamApplicative" should {
    "sequence a list of same length streams" in {
      Applicative.streamApplicative.sequence(
        List(
          Stream(1, 2, 3),
          Stream(4, 5, 6),
          Stream(7, 8, 9),
          Stream(10, 11, 12)
        )
      ).toList should be(
        List(
          List(1, 4, 7, 10),
          List(2, 5, 8, 11),
          List(3, 6, 9, 12)
        )
      )
    }

    "sequence a list of different lenght streams" in {
      Applicative.streamApplicative.sequence(
        List(
          Stream(1, 2, 3),
          Stream(4, 5),
          Stream(6, 7, 8, 9),
          Stream(10, 11, 12)
        )
      ).toList should be(
        List(
          List(1, 4, 6, 10),
          List(2, 5, 7, 11)
        )
      )
    }

    "sequence a list of streams with an empty stream" in {
      Applicative.streamApplicative.sequence(
        List(
          Stream(1, 2, 3),
          Stream(),
          Stream(6, 7, 8, 9),
          Stream(10, 11, 12)
        )
      ).toList should be(
        List()
      )
    }
  }
}