package nl.hugo.redbook.ch8

class Exercise02 {
  trait Gen[A]
  object Gen {
    def choose(from: Int, to: Int): Gen[Int] = ???
    def listOf[A](g: Gen[A]): Gen[List[A]] = ???
  }
  def forAll[A](g: Gen[A])(p: A => Boolean): Boolean = ???

  val max: List[Int] => Int = ???

  val int = Gen.choose(0, 100)
  val intList = Gen.listOf(int)
  def constList(i: Int) = Gen.listOf(Gen.choose(i, i + 1))

  val props = Seq(
    forAll(int) { i =>
      forAll(constList(i)) { l =>
        max(l) == i
      }
    },
    forAll(intList) { l =>
      max(l.reverse) == max(l)
    },
    forAll(intList) { l =>
      max(l ++ l) == max(l)
    },
    forAll(intList) { l1 =>
      forAll(intList) { l2 =>
        max(l1 ++ l2) == (max(l1) max max(l2))
      }
    }
  )
}