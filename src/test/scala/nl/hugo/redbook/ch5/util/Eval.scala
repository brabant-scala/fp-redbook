package nl.hugo.redbook.ch5.util

import nl.hugo.redbook.Warn
import nl.hugo.redbook.ch5.Empty
import nl.hugo.redbook.ch5.Stream
import scala.language.implicitConversions

object Eval {

  def apply()(implicit warn: Warn = Warn.Silent): Eval = new Eval(warn)

  // class Val is needed to create a Stream with by-name values
  class Val[A](a: => A) {
    private[Eval] def value: A = a
  }

  object Val {
    // implicit conversion is used to convert a by-name value into a value without evaluating it
    import language.implicitConversions
    implicit def any2val[A](a: => A): Val[A] = new Val(a)
  }
}

class Eval private (warn: Warn) {
  import Eval._

  private var count = 0

  def expect(expected: Int): Unit =
    if (count > expected) {
      warn(s"$count evaluation(s) is less than optimal ($expected)")
    } else if (count < expected)
      warn(s"$count evaluation(s) is better than expected ($expected)")

  def apply[A](a: => A): A = {
    count += 1
    a
  }

  // using Val is necessary because varargs cannot be by-name
  def stream[A](vs: Val[A]*): Stream[A] =
    if (vs.isEmpty) Empty
    else Stream.cons(apply(vs.head.value), stream(vs.tail: _*))
}