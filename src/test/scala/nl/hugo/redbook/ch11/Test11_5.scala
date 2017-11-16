package nl.hugo.redbook.ch11

import org.scalatest.{ Matchers, WordSpec }

import nl.hugo.redbook.ch11.Monad._

class Test11_05 extends WordSpec with Matchers {
  "replicateM" should {
    "work on List[_]" in {
      listMonad.replicateM(3, List.empty[Int]) should be(List.empty[List[Int]])

      listMonad.replicateM(3, List(1)) should be(List(List(1, 1, 1)))
      listMonad.replicateM(0, List(1)) should be(List(List.empty[Int]))

      listMonad.replicateM(0, List(1, 2, 3)) should be(List(List.empty[Int]))
      listMonad.replicateM(2, List(1, 2, 3)) should be(
        List(
          List(1, 1),
          List(1, 2),
          List(1, 3),

          List(2, 1),
          List(2, 2),
          List(2, 3),

          List(3, 1),
          List(3, 2),
          List(3, 3)
        )
      )
    }
    "work on Option[_]" in {
      optionMonad.replicateM(3, Some(1)) should be(Some(List(1, 1, 1)))
      optionMonad.replicateM(3, None) should be(None)
      optionMonad.replicateM(0, Some(1)) should be(Some(List.empty[Int]))
      optionMonad.replicateM(0, None) should be(Some(List.empty[Int]))
    }
  }
}