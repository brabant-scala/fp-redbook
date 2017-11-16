package nl.hugo.redbook.ch11

import nl.hugo.redbook.ch11.Monad._
import org.scalatest.{Matchers, WordSpec}

class Test11_17_Aad extends WordSpec with Matchers {
  val m = idMonad

  "idMonad.unit" should {
    "return Id with given value" in {
      m.unit(10) shouldBe Id(10)
    }
  }

  "idMonad.flatMap" should {
    "return mapped value into new Id type" in {
      m.flatMap(m.unit(10))(x => m.unit((x / 2).toString)) shouldBe Id("5")
    }
  }
}

