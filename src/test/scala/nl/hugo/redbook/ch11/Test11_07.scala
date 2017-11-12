package nl.hugo.redbook.ch11

import nl.hugo.redbook.ch11.Monad._
import org.scalatest.{Matchers, WordSpec}

class Test11_07 extends WordSpec with Matchers {
  val m = optionMonad

  "Monad.compose" should {
    "return Some(List(...)) when input is Some" in {
      m.compose[Int,Int,Int](a => Some(a + 1), b => Some(10 * b))(2) shouldBe Some(30)
    }

    "return Some(List(...)) without the entries for which the filter returns Some(false)" in {
      m.compose[Int,Int,Int](_ => None, b => Some(10 * b))(2) shouldBe None
    }

    "return None when at least one of the filter outputs is None" in {
      m.compose[Int,Int,Int](a => Some(a + 1), _ => None)(2) shouldBe None
    }
  }
}

