package nl.hugo.redbook.ch2

object Exercise01 {
  def fib(n: Int): Int = {
    def fib(x: Int, y: Int, n: Int): Int = {
      if (n == 0) x
      else fib(y, x + y, n - 1)
    }
    fib(0, 1, n)
  }
}