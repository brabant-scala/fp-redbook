package nl.hugo.redbook.ch3

object Exercise17 {
  import List.foldRight

  def toString(as: List[Double]): List[String] = foldRight(as, Nil: List[String])((current, result) => Cons(current.toString, result))
}
