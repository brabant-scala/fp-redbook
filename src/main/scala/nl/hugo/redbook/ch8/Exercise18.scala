package nl.hugo.redbook.ch8

import nl.hugo.redbook.ch7.Par
import nl.hugo.redbook.ch7.Par.Par

object Exercise18 {

  val smallInt = Gen.choose(-10, 10)

  def takeWhileProp(f: Int => Boolean) = Prop.forAll(Gen.listOf(smallInt)) { ns =>
    val take = ns.takeWhile(f)
    val drop = ns.dropWhile(f)
    take.forall(f) &&
      !drop.headOption.exists(f) &&
      (take.size <= ns.size) &&
      (take ::: drop == ns)
  }
}