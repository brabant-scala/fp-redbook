package nl.hugo.redbook.ch11

import nl.hugo.redbook.ch11.Monad._
import nl.hugo.redbook.ch6.State
import org.scalatest.{ Matchers, WordSpec }

class Test11_18 extends WordSpec with Matchers {
  "replicateM" should {
    "work on State[_]" in {
      val M = stateMonad[Int]

      M.replicateM(0, State.unit("A")).run.apply(1) should be(List(), 1)
      M.replicateM(1, State.unit("A")).run.apply(1) should be(List("A"), 1)
      M.replicateM(2, State.unit("A")).run.apply(1) should be(List("A", "A"), 1)

      M.map2(
        State.unit("High"),
        State.unit(5)
      ) {
          (str, n) => s"$str $n"
        }.run(120) should be(State.unit("High 5").run(120))

      M.sequence(
        List[State[Int, String]](
          State.unit("A"),
          State.unit("B"),
          State.unit("C"),
          State.unit("D")
        )
      ).run(1) should be(
          State.unit[Int, List[String]](
            List("A", "B", "C", "D")
          ).run(1)
        )
    }
  }
}