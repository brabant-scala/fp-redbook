package nl.hugo.redbook.ch11

import nl.hugo.redbook.ch4.{None, Some}
import nl.hugo.redbook.ch11.Monad._

import org.scalatest.{Matchers, WordSpec}

class Test11_04_Aad extends WordSpec with Matchers {
  val m = optionMonad

  "Monad.replicateM" should {
    "return Some(List(...)) when input is Some" in {
      m.replicateM(3, Some(1)) shouldBe Some(List(1,1,1))
    }

    "return None when input is None" in {
      m.replicateM(3, None) shouldBe None
    }

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
  }
}

