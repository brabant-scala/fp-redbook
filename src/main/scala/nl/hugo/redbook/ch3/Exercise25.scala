package nl.hugo.redbook.ch3

object Exercise25 {
  def size[A](t: Tree[A]): Int =  t match{
    case Leaf(value) => 1
    case Branch(l,f) => size(l) + size(f)
  }

}