package nl.hugo.redbook.ch10

object Helpers {
  def appendR(c: Char, s: String): String = s :+ c

  def appendL(s: String, c: Char): String = s :+ c
}
