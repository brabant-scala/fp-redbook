package nl.hugo.redbook.ch7

object Exercise07 {

  // exercise 7.7
  // (1) map(y)(id) == id =>
  // (2) map(unit(x))(f) == unit(f(x))
  // (3) y = unit(x)
  // (4) h = f compose g
  //
  // map(map(y)(g))(f) == (3)
  // map(map(unit(x))(g))(f) == (2)
  // map(unit(g(x)))(f) == (2)
  // unit(f(g(x))) == (4)
  // unit(h(x)) == (2)
  // map(unit(x))(h) == (4)
  // map(unit(x))(f compose g) == (3)
  // map(y)(f compose g)
}