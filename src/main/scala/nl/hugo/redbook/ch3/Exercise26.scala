package nl.hugo.redbook.ch3

object Exercise26 {
  def maximum(t: Tree[Int]): Int = t match {
    case Branch(l, r) => {
      val lv = maximum(l)
      val rv = maximum(r)
      if (lv > rv) {
        lv
      } else {
        rv
      }
    }
    case Leaf(l) => l
  }
}