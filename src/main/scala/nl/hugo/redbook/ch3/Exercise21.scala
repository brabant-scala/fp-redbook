package nl.hugo.redbook.ch3

object Exercise21 {
  import List.flatMap

  def filter[A](l: List[A])(p: A => Boolean): List[A] =
    flatMap(l)(a => if (p(a)) List(a) else Nil)
}