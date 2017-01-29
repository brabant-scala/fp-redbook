package nl.hugo.redbook.ch3

object Exercise22 {
  // Assumption: if one list is shorter than the other, the result will have the shortest length
  def add1(l: List[Int], r: List[Int]): List[Int] = (l,r) match {
    case (_  , Nil)               => Nil
    case (Nil, _)                 => Nil
    case (Cons(l,ls), Cons(r,rs)) => Cons(l+r, add(ls,rs))
  }
  def add(l: List[Int], r: List[Int]): List[Int] = (l,r) match {
    case (Cons(l,ls), Cons(r,rs)) => Cons(l+r, add(ls,rs))
    case _                        => Nil
  }
}
