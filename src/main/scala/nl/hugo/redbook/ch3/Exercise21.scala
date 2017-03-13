package nl.hugo.redbook.ch3

object Exercise21 {
  import List._
  // exercise 3.21
  def filter[A](as: List[A])(p: A => Boolean): List[A] = flatMap(as)(a =>
    if (p(a)) {
      List(a)
    } else {
      List[A]()
    }
  )
}