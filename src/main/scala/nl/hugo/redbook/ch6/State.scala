package nl.hugo.redbook.ch6

case class State[A,S](run: S => (A,S)) {

  def unit(a: A): S => (A,S) =
    s => (a, s)

  def map[B](s: S)(f: A => B): S => (B,S) = {
    flatMap(s) { a => s2 =>
      (f(a), s2)
    }
  }

//  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
//    val (a, rngA) = f(rng)
//    g(a)(rngA)
//  }

}
