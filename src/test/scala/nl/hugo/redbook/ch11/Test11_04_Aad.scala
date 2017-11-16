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
  }
}

