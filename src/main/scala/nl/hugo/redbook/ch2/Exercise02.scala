package nl.hugo.redbook.ch2

import scala.annotation.tailrec

object Exercise02 {
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def check(i: Int): Boolean = {
      if (i + 1 < as.length) {
        if (ordered(as(i), as(i + 1))) {
          check(i + 1)
        } else false
      } else true
    }
    check(0)
  }
}