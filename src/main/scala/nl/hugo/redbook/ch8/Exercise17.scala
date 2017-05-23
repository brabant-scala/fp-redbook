package nl.hugo.redbook.ch8

import nl.hugo.redbook.ch7.Par

object Exercise17 {
  import Exercise16.parInt

  val forkProp = Prop.forAllPar(parInt) { i =>
    Prop.equal(Par.fork(i), i)
  }
}