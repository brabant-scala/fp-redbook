package nl.hugo.redbook.ch3

object Exercise11 {
  import List.foldLeft
  def sum(ints: List[Int]): Int = foldLeft(ints, 0)(_ + _)

  def product(ds: List[Double]): Double = foldLeft(ds, 1.0)(_ * _)
}