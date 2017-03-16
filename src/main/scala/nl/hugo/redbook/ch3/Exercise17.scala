package nl.hugo.redbook.ch3

object Exercise17 {
  def toString(as: List[Double]): List[String] = as match {
    case Cons(b, bs) => Cons(b.toString, toString(bs))
    case Nil => Nil
  }
}