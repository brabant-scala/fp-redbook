package nl.hugo.redbook.ch8

trait BooleanProp { self =>
  def check: Boolean

  // Exercise 8.3
  def &&(p: BooleanProp): BooleanProp =
    new BooleanProp {
      def check: Boolean =
        self.check && p.check
    }
}