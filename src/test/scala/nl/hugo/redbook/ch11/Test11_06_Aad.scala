package nl.hugo.redbook.ch11

import nl.hugo.redbook.ch4.{None, Some}
import nl.hugo.redbook.ch11.Monad._

import org.scalatest.{Matchers, WordSpec}

class Test11_06_Aad extends WordSpec with Matchers {
  val m = optionMonad

  "Monad.filterM" should {
    "return Some(List(...)) when input is Some" in {
      m.filterM(List(1, 2, 3))(_ => Some(true)) shouldBe Some(List(1,2,3))
    }

    "return Some(List(...)) without the entries for which the filter returns Some(false)" in {
      m.filterM(List(1, 2, 3)) {
        case 2 => Some(false)
        case _ => Some(true)
      } shouldBe Some(List(1,3))
    }

    "return None when at least one of the filter outputs is None" in {
      m.filterM(List(1, 2, 3)) {
        case 2 => None
        case _ => Some(true)
      } shouldBe None
    }
  }
}

