package nl.hugo.redbook.ch3

import scala.annotation.tailrec

object Exercise24 {
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {

    def compareSubsequence1(supSeq: List[A], subSeq: List[A]): Boolean = {
      (supSeq, subSeq) match {
        case (_               , Nil             ) => true
        case (Nil             , _               ) => false
        case (Cons(suph, supt), Cons(subh, subt)) => suph == subh && compareSubsequence(supt, subt)
      }
      // TODO Can make this one also tail recursive ()move suoh == subh to left of arrow and move second nil check below it
    }
    @tailrec
    def compareSubsequence(supSeq: List[A], subSeq: List[A]): Boolean = {
      (supSeq, subSeq) match {
        case (Cons(suph, supt), Cons(subh, subt)) if suph == subh => compareSubsequence(supt, subt)
        case (_               , Nil             )                 => true
        case _                                                    => false
      }
    }

    (sup,sub) match {
      case (_               , Nil      )                                => true
      case (Nil             , _        )                                => false
      case (Cons(_   , _   ), Cons(_,_)) if compareSubsequence(sup,sub) => true
      case (Cons(suph, supt), Cons(_,_))                                => hasSubsequence(supt,sub)
    }
  }
}
