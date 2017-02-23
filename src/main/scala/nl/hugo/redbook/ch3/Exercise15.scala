package nl.hugo.redbook.ch3

object Exercise15 {
  import List.{ append, foldLeft, reverse }

  def flatAppend[A](ls: List[List[A]]): List[A] =
    reverse(ls) match {
      case Nil => Nil
      case Cons(h, t) => foldLeft(t, h)((a, b) => append(b, a))
    }
}