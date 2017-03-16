package nl.hugo.redbook.ch6

import org.scalatest._

class Test6_11 extends WordSpec with Matchers {
  "A candymachine" should {
    "unlock a candy when inserting a coin and turning the knob" in {
      State.simulateMachine(List(Coin, Turn)).run(Machine(locked = true, 5, 0))._1 should be((1, 4))
    }

    "unlock a candy when turning the knob on an unlocked machine" in {
      State.simulateMachine(List(Turn)).run(Machine(locked = false, 5, 0))._1 should be((1, 4))
    }

    "do nothing when turning the knob on a locked machine" in {
      State.simulateMachine(List(Turn)).run(Machine(locked = true, 5, 0))._1 should be((0, 5))
    }

    "do nothing when inserting a coin in an unlocked machine" in {
      State.simulateMachine(List(Coin)).run(Machine(locked = false, 5, 0))._1 should be((0, 5))
    }

    "ignore coins when a machine is out of candy" in {
      State.simulateMachine(List(Coin)).run(Machine(locked = true, 0, 0))._1 should be((0, 0))
    }

    "ignore turns when a machine is out of candy" in {
      State.simulateMachine(List(Turn)).run(Machine(locked = false, 0, 0))._1 should be((0, 0))
    }

    "release four candies for four coins" in {
      val inputs = List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)
      val machine = Machine(locked = true, 5, 10)

      State.simulateMachine(inputs).run(machine)._1 should be((14, 1))
    }

  }
}
