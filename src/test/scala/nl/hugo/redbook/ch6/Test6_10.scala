package nl.hugo.redbook.ch6

import nl.hugo.redbook.Spec

class Test6_10 extends Spec {
  import State._

  implicit val last = CNG.zero

  "State.unit" should {

    "return a state action that returns a constant" in {
      val rand: Rand[Int] = unit(42)

      val rng = CNG(0)
      val (n, r) = rand.run(rng)
      n should be(42)
      r should be(rng)
    }
  }

  "State.map" should {

    "convert one action into another" in {
      val rand: Rand[Int] = State(RNG.int).map(_ / 6)

      val rng = CNG(42)
      val (n, r) = rand.run(rng)
      n should be(7)
      r should be(last)
    }
  }

  "State.map2" should {

    val rand1 = State(RNG.int)
    val rand2 = State(RNG.int)

    "combine two actions into one (instance)" in {
      val rand = rand1.map2(rand2)(_ * _)

      val rng = CNG(6, 7)
      val (n, r) = rand.run(rng)
      n should be(42)
      r should be(last)
    }

    "combine two actions into one (companion)" in {
      val rand = rand1.map2(rand2)(_ * _)

      val rng = CNG(6, 7)
      val (n, r) = rand.run(rng)
      n should be(42)
      r should be(last)
    }
  }

  "State.flatMap" should {

    "combine two actions into one" in {
      val rand1 = State(RNG.int)
      def rand2(count: Int) = State(RNG.intsViaSequence(count))
      val rand = rand1.flatMap(rand2)

      val rng = CNG(3, 4, 5, 6)
      val (l, r) = rand.run(rng)
      l.toSet should be(Set(4, 5, 6))
      r should be(last)
    }
  }

  "State.sequence" should {

    "combine multiple actions into one" in {
      val rands = List.fill(3)(State(RNG.int))
      val rand = sequence(rands)

      val rng = CNG(1, 2, 3)
      val (l, r) = rand.run(rng)
      l.toSet should be(Set(1, 2, 3))
      r should be(last)
    }
  }
}

import org.scalatest._

class Test6_10Aad extends WordSpec with Matchers {
  "State" should {
    "be initialized using the unit function" in {
      State.unit(123).run(1) should be((123, 1))
    }

    "map its output using the map function" in {
      State.unit(123).map(_.toString).run(1) should be(("123", 1))
    }

    "combine the output of two states using the map2 function" in {
      State.unit[Int, Int](13).map2(State.unit(17))(_ + _).run(7) should be((30, 7))
    }

    "should apply flatMap" in {
      State.unit(123).flatMap((i: Int) => State.unit[Int, Int](i + 1)).run(11) should be((124, 11))
    }

    "sequence a List of State" in {
      val ints = List[State[Int, Int]](State.unit(1), State.unit(2), State.unit(3))

      State.sequence(ints).run(11) should be((List(1, 2, 3), 11))
    }
  }
}
