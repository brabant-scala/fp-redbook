package nl.hugo.redbook.ch3

object Exercise13 {
  import List.reverse

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    List.foldLeft(reverse(as), z)((b, a) => f(a, b))

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    List.foldRight(reverse(as), z)((a, b) => f(b, a))
}