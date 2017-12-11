package nl.hugo.redbook.ch3

object Exercise27 {

  def depth[A](t: Tree[A]): Int =
    t match {
      case Branch(l, r) => 1 + (depth(l) max depth(r))
      case Leaf(_) => 0
    }
}