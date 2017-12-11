package nl.hugo.redbook.ch2

object Exercise01 {
  def fib(n: Int): Int = {

    @annotation.tailrec
    def go(n: Int, prev: Int, curr: Int): Int = n match {
      case 0 => prev
      case 1 => curr
      case _ => go(n - 1, curr, prev + curr)
    }

    go(n, 0, 1)
  }
}
