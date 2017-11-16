package nl.hugo.redbook.ch11

import org.scalatest.{Matchers, WordSpec}
import nl.hugo.redbook.ch6.{CNG, RNG, State}
import nl.hugo.redbook.ch11.Monad._

class Test11_02_Aad extends WordSpec with Matchers {
  implicit val last = CNG.zero
  val m = stateMonad[RNG]

  "State.unit" should {
    "return a state action that returns a constant" in {
      val rand: State[RNG, Int] = m.unit(42)

      val rng = CNG(0)
      val (n, r) = rand.run(rng)
      n should be(42)
      r should be(rng)
    }
  }

//  "State.flatMap" should {
//    "combine two actions into one" in {
//      val rand1 = State(RNG.int)
//      def rand2(count: Int) = State(RNG.intsViaSequence(count))
//      val rand = rand1.flatMap(rand2)
//
//      val rand1b = m.unit(RNG.int)
//      def rand2b(count: Int) = m.replicateM(count, State(RNG.int))
//      val randb = m.flatMap(rand1b)(a => rand2b(a))
//
//      val rng = CNG(3, 4, 5, 6)
//      val (l, r) = rand.run(rng)
//      l.toSet should be(Set(4, 5, 6))
//      r should be(last)
//    }
//  }
}
