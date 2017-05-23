package nl.hugo.redbook.ch8

class Exercise01 {
  trait Gen[A]
  object Gen {
    def choose(from: Int, to: Int): Gen[Int] = ???
    def listOf[A](g: Gen[A]): Gen[List[A]] = ???
  }
  def forAll[A](g: Gen[A])(p: A => Boolean): Boolean = ???

  val sum: List[Int] => Int = ???

  val int = Gen.choose(0, 100)
  val intList = Gen.listOf(int)
  def constList(i: Int) = Gen.listOf(Gen.choose(i, i + 1))

  val basicProps = Seq(
    // base case
    forAll(constList(0)) { l =>
      sum(l) == 0
    },
    // inductive step
    forAll(intList) { l =>
      forAll(int) { i =>
        sum(i :: l) == i + sum(l)
      }
    }
  )
  val moreProps = Seq(
    forAll(intList) { l =>
      sum(l) == sum(l.reverse)
    },
    forAll(intList) { l =>
      sum(l ++ l) == 2 * sum(l)
    },
    forAll(intList) { l1 =>
      forAll(intList) { l2 =>
        sum(l1 ++ l2) == sum(l2 ++ l1)
      }
    },
    forAll(intList) { l1 =>
      forAll(intList) { l2 =>
        sum(l1 ++ l2) == sum(l1) + sum(l2)
      }
    },
    forAll(int) { i =>
      forAll(constList(i)) { l =>
        sum(l) == l.size * i
      }
    }
  )
}