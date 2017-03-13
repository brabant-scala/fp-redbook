package nl.hugo.redbook.ch3

import nl.hugo.redbook.ch3.Cons
object Exercise16 {
  def add(v: Int)(as: List[Int]): List[Int] = as match{
    case Cons(b, bs) => Cons((b+v), add(v)(bs))
    case Nil => Nil
  }
}