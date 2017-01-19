package nl.hugo.redbook.ch3

object Exercise15 {
  def flatAppend[A](ls: List[List[A]]): List[A] = List.foldRight(ls, List[A]())(List.append)
}