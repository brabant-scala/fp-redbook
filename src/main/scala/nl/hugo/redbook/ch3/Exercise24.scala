package nl.hugo.redbook.ch3

object Exercise24 {
  import List.{ contains, foldRight }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {

    @annotation.tailrec
    def loop(list: List[A], matches: List[List[A]]): Boolean =
      if (contains(matches, Nil)) true
      else
        list match {
          case Nil => sub == Nil
          case Cons(head, tail) =>
            val newMatches = Cons(sub, matches)
            val nextMatches = collect(newMatches) {
              case Cons(h, t) if h == head => t
              case Nil => Nil // for hasSubsequence(_, Nil)
            }
            loop(tail, nextMatches)
        }

    loop(sup, Nil)
  }

  def collect[A, B](as: List[A])(f: PartialFunction[A, B]): List[B] =
    foldRight(as, Nil: List[B]) { (x, xs) =>
      if (f.isDefinedAt(x)) Cons(f(x), xs)
      else xs
    }
}