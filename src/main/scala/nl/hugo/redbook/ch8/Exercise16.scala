package nl.hugo.redbook.ch8

import nl.hugo.redbook.ch7.Par
import nl.hugo.redbook.ch7.Par.Par

object Exercise16 {

  val parInt: Gen[Par[Int]] = Gen.choose(-100, 100).listOfN(Gen.choose(0, 10)).map { l =>
    l.foldLeft(Par.unit(0)) { (p, i) =>
      Par.fork {
        Par.map2(p, Par.unit(i))(_ + _)
      }
    }
  }
}