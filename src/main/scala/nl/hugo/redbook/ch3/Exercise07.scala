package nl.hugo.redbook.ch3

object Exercise07 {

  def foldRightLazy[A, B](as: List[A], z: B)(f: (A, => B) => B): B =
    as match {
      case Cons(x, xs) => f(x, foldRightLazy(xs, z)(f))
      case Nil => z
    }
}