package nl.hugo.redbook.ch6

import nl.hugo.redbook.Spec

class Test6_11 extends Spec {
  import State.simulateMachine

  val buy = List(Coin, Turn)

  "simulateMachine" should {

    "accept a coin when locked" in {
      val simulation = simulateMachine(List(Coin))

      val machine = Machine(locked = true, candies = 1, coins = 0)
      val ((coins2, candies2), updated) = simulation.run(machine)
      coins2 should be(1)
      candies2 should be(1)
      updated should be(Machine(locked = false, candies = 1, coins = 1))
    }

    "refuse a coin when not locked" in {
      val simulation = simulateMachine(List(Coin))

      val machine = Machine(locked = false, candies = 1, coins = 0)
      val ((coins2, candies2), updated) = simulation.run(machine)
      coins2 should be(0)
      candies2 should be(1)
      updated should be(machine)
    }

    "accept a turn when not locked" in {
      val simulation = simulateMachine(List(Turn))

      val machine = Machine(locked = false, candies = 1, coins = 1)
      val ((coins2, candies2), updated) = simulation.run(machine)
      coins2 should be(1)
      candies2 should be(0)
      updated should be(Machine(locked = true, candies = 0, coins = 1))
    }

    "refuse a turn when locked" in {
      val simulation = simulateMachine(List(Turn))

      val machine = Machine(locked = true, candies = 1, coins = 0)
      val ((coins2, candies2), updated) = simulation.run(machine)
      coins2 should be(0)
      candies2 should be(1)
      updated should be(machine)
    }

    "refuse a coin when empty" in {
      val simulation = simulateMachine(List(Coin))

      val machine = Machine(locked = true, candies = 0, coins = 0)
      val ((coins2, candies2), updated) = simulation.run(machine)
      coins2 should be(0)
      candies2 should be(0)
      updated should be(machine)
    }

    "refuse a turn when empty" in {
      val simulation = simulateMachine(List(Turn))

      val machine = Machine(locked = false, candies = 0, coins = 0)
      val ((coins2, candies2), updated) = simulation.run(machine)
      coins2 should be(0)
      candies2 should be(0)
      updated should be(machine)
    }

    "simulate the machine as specified in the book" in {
      val input = List.fill(4)(buy).flatten
      val simulation = simulateMachine(input)

      val machine = Machine(locked = true, candies = 5, coins = 10)
      val ((coins2, candies2), updated) = simulation.run(machine)
      coins2 should be(14)
      candies2 should be(1)
      updated should be(Machine(locked = true, candies = 1, coins = 14))
    }
  }
}