package nl.hugo.redbook.ch11

import nl.hugo.redbook.ch11.Monad._
import org.scalatest.{ Matchers, WordSpec }

class Test11_06 extends WordSpec with Matchers {
  "filterM" should {

    "work on List[_]" in {
      listMonad.filterM(List(1, 2, 3, 4, 5, 6))(x => List(false)) should be(List(List.empty[Int]))
      listMonad.filterM(List(1, 2, 3, 4, 5, 6))(x => List(true)) should be(List(List(1, 2, 3, 4, 5, 6)))
      listMonad.filterM(List(1, 2, 3, 4, 5, 6))(x => List(x % 2 == 0)) should be(List(List(2, 4, 6)))
      listMonad.filterM(List(1, 2, 3))(x => List(true, false)) should be(
        List(
          List(1, 2, 3),
          List(1, 2),
          List(1, 3),
          List(1),
          List(2, 3),
          List(2),
          List(3),
          List()
        )
      )
      listMonad.filterM(List(1, 2, 3))(x => List(false, true)) should be(
        List(
          List(),
          List(3),
          List(2),
          List(2, 3),
          List(1),
          List(1, 3),
          List(1, 2),
          List(1, 2, 3)
        )
      )
    }
    "work on Option[_]" in {
      optionMonad.filterM(List(1, 2, 3, 4, 5, 6))(_ => Some(true)) should be(Some(List(1, 2, 3, 4, 5, 6)))
      optionMonad.filterM(List(1, 2, 3, 4, 5, 6))(_ => Some(false)) should be(Some(List()))
      optionMonad.filterM(List(1, 2, 3, 4, 5, 6))(x => Some(x % 2 == 0)) should be(Some(List(2, 4, 6)))
    }
  }
}