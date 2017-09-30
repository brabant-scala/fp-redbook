package nl.hugo.redbook.ch10

import nl.hugo.redbook.ch10.Monoid._
import nl.hugo.redbook.ch8.Gen
import org.scalatest.{ Matchers, WordSpec }

class Test10_10 extends WordSpec with Matchers {
  "wcMonoid" should {
    val emptyStub = Stub("")
    val stub = Stub("S")
    val part = Part("L", 3, "R")
    val emptyLStub = Part("", 7, "r")
    val emptyRStub = Part("l", 11, "")

    "combine two stubs" in {
      wcMonoid.op(stub, stub) shouldBe Stub("SS")
    }

    "combine two empty stubs" in {
      wcMonoid.op(emptyStub, emptyStub) shouldBe emptyStub
    }

    "combine an empty stub with a stub" in {
      wcMonoid.op(emptyStub, stub) shouldBe stub
    }

    "combine a stub and an empty stub" in {
      wcMonoid.op(stub, emptyStub) shouldBe stub
    }

    "combine a stub and an lstub" in {
      wcMonoid.op(stub, part) shouldBe Part("SL", 3, "R")
    }

    "combine an rstub and a stub" in {
      wcMonoid.op(part, stub) shouldBe Part("L", 3, "RS")
    }

    "combine and empty stub with an lstub" in {
      wcMonoid.op(emptyStub, part) shouldBe part
    }

    "combine and rstub with an empty stub" in {
      wcMonoid.op(part, emptyStub) shouldBe part
    }

    "combine and empty stub with an empty lstub" in {
      wcMonoid.op(emptyStub, emptyLStub) shouldBe emptyLStub
    }

    "combine and empty rstub with an empty stub" in {
      wcMonoid.op(emptyRStub, emptyStub) shouldBe emptyRStub
    }

    "combine an rstub with an empty lstub" in {
      wcMonoid.op(part, emptyLStub) shouldBe Part("L", 3 + 7 + 1, "r")
    }

    "combine an empty rstub with an lstub" in {
      wcMonoid.op(emptyRStub, part) shouldBe Part("l", 11 + 3 + 1, "R")
    }

    "combine an empty rstub with an empty lstub" in {
      wcMonoid.op(emptyRStub, emptyLStub) shouldBe Part("l", 7 + 11, "r")
    }

    "obeys the monoid laws" in {
      val nonSpaceCharGen: Gen[Char] = Gen.choose(33, 127).map(_.toChar)

      def stringGen(l: Int): Gen[String] = Gen.listOfN(l, nonSpaceCharGen).map(_.mkString)

      val stubGen: Gen[Stub] =
        for {
          l <- Gen.choose(0, 5)
          v <- stringGen(l)
        } yield Stub(v)

      val partGen: Gen[Part] =
        for {
          ll <- Gen.choose(0, 5)
          ls <- stringGen(ll)
          c <- Gen.choose(0, 5)
          lr <- Gen.choose(0, 5)
          rs <- stringGen(lr)
        } yield Part(ls, c, rs)

      val wcGen = Gen.union(stubGen, partGen)

      PropRunner.run(monoidLaws(wcMonoid, wcGen)) shouldBe true
    }
  }
}