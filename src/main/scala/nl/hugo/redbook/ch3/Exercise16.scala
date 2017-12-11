package nl.hugo.redbook.ch3

object Exercise16 {
  import List.foldRight

  def add(v: Int)(as: List[Int]): List[Int] =
    foldRight(as, Nil: List[Int])((x, xs) => Cons(x + v, xs))
}