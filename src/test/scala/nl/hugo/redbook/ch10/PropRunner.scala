package nl.hugo.redbook.ch10

import nl.hugo.redbook.ch6.RNG
import nl.hugo.redbook.ch8.Prop
import nl.hugo.redbook.ch8.Prop.{ Falsified, Passed, Proved }

/** Created by Ardjan on 04/07/2017.
  */
object PropRunner {
  // Reimplemented here, because the version in Gen has Unit as return type.
  def run(
    p: Prop,
    maxSize: Int = 100,
    testCases: Int = 100,
    rng: RNG = RNG.Simple(System.currentTimeMillis)
  ): Boolean =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
        false
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
        true
      case Proved =>
        println(s"+ OK, proved property.")
        true
    }
}
