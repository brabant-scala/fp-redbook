package nl.hugo.redbook.ch8

trait BooleanProp {
  def check: Boolean

  // Exercise 8.3
  def &&(p: BooleanProp): BooleanProp = // if (check) p else this // this is not working. check will only work once
    this.check && p.check
}
