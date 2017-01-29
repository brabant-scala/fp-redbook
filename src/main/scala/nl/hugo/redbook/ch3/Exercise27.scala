package nl.hugo.redbook.ch3

object Exercise27 {
  def depth1[A](t: Tree[A]): Int = {
    def branchDepth(t: Tree[A], currentDepth: Int): Int = {
      t match {
        case Leaf(_)      => currentDepth
        case Branch(l, r) => branchDepth(l, currentDepth + 1) max branchDepth(r, currentDepth + 1)
      }
    }
    branchDepth(t, 0)
  }
  // TODO Ardjan heeft een kortere oplossing. Book says not to include Leaf in the depth calculation
  def depth[A](t: Tree[A]): Int = {
    Tree.fold(t)(_ => 0)((l,r) => 1 + (l max r))
  }
}
