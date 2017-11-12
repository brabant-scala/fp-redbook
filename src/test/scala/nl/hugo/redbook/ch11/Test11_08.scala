package nl.hugo.redbook.ch11

import nl.hugo.redbook.ch11.Monad._
import org.scalatest.{Matchers, WordSpec}

class Test11_08 extends WordSpec with Matchers {
  val m = listMonad

  "listMonad._flatMap" should {
    "return a single List when flat mapping over a single element with method that returns a list" in {
      m._flatMap(m.unit(10))(v => List(v * 10)) shouldBe List(100)
    }

    "return single List when flat mapping over multiple elements with method that returns a list" in {
      m._flatMap(List(1,2,3))(vs => List(vs * 10)) shouldBe List(10,20,30)
    }
  }
}

