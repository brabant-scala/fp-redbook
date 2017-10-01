package nl.hugo.redbook.ch10

import nl.hugo.redbook.ch10.Monoid._
import org.scalatest.{ Matchers, WordSpec }

class Test10_09 extends WordSpec with Matchers {
  "Monoid.isOrdered" should {
    "return true when a sequence is ordered" in {
      val l = IndexedSeq(-10, 0, 10)

      ordered(l) shouldBe true
    }

    "return true when a sequence has duplicate values" in {
      val l = IndexedSeq(-10, 0, 0, 0, 10)

      ordered(l) shouldBe true
    }

    "return false when a sequence is not ordered" in {
      val l = IndexedSeq(10, 0, 10)

      ordered(l) shouldBe false
    }

    "return true when there is a single item in the sequence" in {
      val l = IndexedSeq(0)

      ordered(l) shouldBe true
    }

    // As always, it is debatable whether an empty sequence is ordered, not ordered or whether this sequence should
    // throw an exception. However, given that the return type is a Boolean, and not an Option[Boolean], one cold argue
    // that this function should always return a Boolean and never throw. Then, is an empty sequence 'out of order'?
    "return true on an empty sequence" in {
      ordered(IndexedSeq.empty) shouldBe true
    }
  }
}