package nl.hugo.redbook.ch11

import nl.hugo.redbook.ch4.{None, Some}
import nl.hugo.redbook.ch11.Monad._

import org.scalatest.{Matchers, WordSpec}

class Test11_03_Aad extends WordSpec with Matchers {
  val m = optionMonad

  "Monad.sequence" should {
    "return Some(List(...)) when all input list elements are Some" in {
      m.sequence(List(Some(1),Some(2),Some(3))) shouldBe Some(List(1,2,3))
    }

    "return None when at least one input list element is None" in {
      m.sequence(List(Some(1),None,Some(3))) shouldBe None
    }
  }

  "Monad.traverse" should {
    "return Some(List(...)) when all input list element transforms are some" in {
      m.traverse(List(1,2,3))(Some(_)) shouldBe Some(List(1,2,3))
    }

    "return None when at least one input list element transform is None" in {
      m.traverse(List(1,2,3))(x => if (x != 2) Some(x) else None) shouldBe None
    }
  }
}

