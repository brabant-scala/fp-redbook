package nl.hugo.redbook.ch3

object Exercise11 {
  import List.foldLeft

  def sum(ints: List[Int]): Int = foldLeft(ints, 0)((result, current) => result + current)

  def product(ds: List[Double]): Double = foldLeft(ds, 1.0)((result, current) => result * current)

  def length[A](list: List[A]): Int = foldLeft(list, 0)((result, _) => result + 1)
}
