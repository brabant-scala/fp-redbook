package nl.hugo.redbook.ch2

object Exercise04 {
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)
}