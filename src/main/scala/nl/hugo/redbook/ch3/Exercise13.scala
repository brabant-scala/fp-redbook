package nl.hugo.redbook.ch3

object Exercise13 {
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    def g(b: B, a: A) = f(a,b)
    List.foldLeft(List.reverse(as), z)(g)
  }

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    def g(a: A, b: B) = f(b,a)
    List.foldRight(List.reverse(as), z)(g)
  }

  // NOTE: both should use reverse because otherwise the lists are in
  // a reverse order when using foldLeft and Exercise13.foldLeft
  //TODO
}
