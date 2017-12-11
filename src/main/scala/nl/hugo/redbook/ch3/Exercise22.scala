package nl.hugo.redbook.ch3

object Exercise22 {
  import List.{ foldLeft, reverse }

  def add(l: List[Int], r: List[Int]): List[Int] =
    add_1(l, r)

  def add_1(l: List[Int], r: List[Int]): List[Int] = {

    @annotation.tailrec
    def loop(as: List[Int], bs: List[Int], acc: List[Int]): List[Int] =
      as match {
        case Nil => acc
        case Cons(i, is) => bs match {
          case Nil => acc
          case Cons(j, js) => loop(is, js, Cons(i + j, acc))
        }
      }

    reverse(loop(l, r, Nil))
  }

  // non tail-recursive
  def add_2(l: List[Int], r: List[Int]): List[Int] = l match {
    case Nil => Nil
    case Cons(i, is) => r match {
      case Nil => Nil
      case Cons(j, js) => Cons(i + j, add_2(is, js))
    }
  }

  // unreadable version based on foldLeft (less efficient if l.size > r.size)
  def add_3(l: List[Int], r: List[Int]): List[Int] =
    reverse(foldLeft(l, (r, Nil: List[Int])) {
      case ((Cons(b, bs), acc), a) => (bs, Cons(a + b, acc))
      case ((Nil, acc), a) => (Nil, acc)
    }._2)
}