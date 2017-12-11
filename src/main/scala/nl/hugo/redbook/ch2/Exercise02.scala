package nl.hugo.redbook.ch2

object Exercise02 {

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {

    @annotation.tailrec
    def loop(i: Int): Boolean =
      if (i >= as.length) true
      else if (!ordered(as(i - 1), as(i))) false
      else loop(i + 1)

    loop(1)
  }
}