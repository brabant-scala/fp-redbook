package nl.hugo.redbook.ch3

object Exercise27 {
  def depth[A](t: Tree[A]): Int = t match{
    case Branch(l,r) => {
      val dl = 1 + depth(l)
      val dr = 1 + depth(r)
      if(dl > dr) dl else dr
    }
    case Leaf(l) => 1
  }

}