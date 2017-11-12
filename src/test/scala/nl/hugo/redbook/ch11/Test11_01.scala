package nl.hugo.redbook.ch11

import org.scalatest.{ Matchers, WordSpec }

import nl.hugo.redbook.ch11.Monad._

class Test11_01 extends WordSpec with Matchers {
  "An Option monad" should {
    val m = optionMonad
    "return the value passed in 'unit' as the value in the option" in {
      m.unit(10) shouldBe Some(10)
    }

    "return the resulting value in a single option when flat mapping with a method that itself returns an option" in {
      m.flatMap(m.unit(10))(v => Option(v * 10)) shouldBe Some(100)
    }

    "return None when flat mapping with a unit that results in None" in {
      m.flatMap(m.unit(null: String))(v => Option(v + " foo")) shouldBe None
    }
  }

  "A Stream monad" should {
    val m = streamMonad
    "return the value passed in 'unit' as the only value in the stream" in {
      m.unit(10).toList shouldBe List(10)
    }

    "return a single List when flat mapping over a single element with method that returns a list" in {
      m.flatMap(m.unit(10))(v => Stream(v * 10)).toList shouldBe List(100)
    }

    "return single List when flat mapping over multiple elements with method that returns a list" in {
      m.flatMap(Stream(1,2,3))(vs => Stream(vs * 10)).toList shouldBe List(10,20,30)
    }
  }

  "A List monad" should {
    val m = listMonad
    "return the value passed in 'unit' as the only value in the list" in {
      m.unit(10) shouldBe List(10)
    }

    "return a single List when flat mapping over a single element with method that returns a list" in {
      m.flatMap(m.unit(10))(v => List(v * 10)) shouldBe List(100)
    }

    "return single List when flat mapping over multiple elements with method that returns a list" in {
      m.flatMap(List(1,2,3))(vs => List(vs * 10)) shouldBe List(10,20,30)
    }
  }
}

