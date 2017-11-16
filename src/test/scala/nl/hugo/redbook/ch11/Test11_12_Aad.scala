package nl.hugo.redbook.ch11

import nl.hugo.redbook.ch11.Monad._

import org.scalatest.{Matchers, WordSpec}

class Test11_12_Aad extends WordSpec with Matchers {
  val m = listMonad

  "listMonad.join" should {
    "return a single List when joining a list of lists with all values" in {
      m.join(List(List(1,2),List(3,4))) shouldBe List(1,2,3,4)
    }

    "return a single List when joining a list of lists where one is empty" in {
      m.join(List(List(1,2),List.empty)) shouldBe List(1,2)
    }

    "return an empty List when joining an empty list of lists" in {
      m.join(List.empty) shouldBe List.empty
    }
  }
}

