package nl.hugo.redbook.ch6

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {

  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    override def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed)
      // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  // Exercise 6.01
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (v, r) = rng.nextInt

    (v match {
      case Int.MinValue => 0
      case s if s < 0 => -s
      case s => s
    }, r)
  }

  // Exercise 6.02
  def double(rng: RNG): (Double, RNG) = {
    val (v, r) = nonNegativeInt(rng)

    (v / (Int.MaxValue.toDouble + 1), r)
  }

  // Exercise 6.03
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r2) = rng.nextInt
    val (d, r3) = RNG.double(r2)

    ((i, d), r3)
  }

  // Exercise 6.03
  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, r2) = RNG.double(rng)
    val (i, r3) = r2.nextInt

    ((d, i), r3)
  }

  // Exercise 6.03
  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d0, r2) = RNG.double(rng)
    val (d1, r3) = RNG.double(r2)
    val (d2, r4) = RNG.double(r3)

    ((d0, d1, d2), r4)
  }

  // Exercise 6.04
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count > 0) {
      val (i, r) = rng.nextInt
      val (l, r2) = RNG.ints(count - 1)(r)
      (i :: l, r2)
    } else
      (List(), rng)

  }

  // Exercise 6.05
  def doubleViaMap: Rand[Double] = map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  // Exercise 6.06
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (va, rng2) = ra(rng)
      val (vb, rng3) = rb(rng2)
      (f(va, vb), rng3)
    }

  // Exercise 6.07
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  // Exercise 6.07
  def intsViaSequence(count: Int): Rand[List[Int]] = ???

  // Exercise 6.08
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng =>
      val (v, rng2) = f(rng)
      g(v)(rng2)
  }

  // Exercise 6.08
  def nonNegativeLessThan(n: Int): Rand[Int] = {
    rng =>
      RNG.flatMap(nonNegativeInt) { i: Int =>
        val mod = i % n
        if (i + (n - 1) - mod >= 0)
          RNG.unit(mod)
        else
          nonNegativeLessThan(n)
      }(rng)
  }
  // Exercise 6.09
  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    RNG.flatMap(s)(a => RNG.unit(f(a)))

  // Exercise 6.09
  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    RNG.flatMap(ra) { a =>
      RNG.map(rb)(b => f(a, b))
    }
}

case class State[S, +A](run: S => (A, S)) {
  // Exercise 6.10
  def map[B](f: A => B): State[S, B] =
    flatMap { a => State.unit(f(a)) }

  // Exercise 6.10
  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap { a =>
      sb.map { b => f(a, b) }
    }

  // Exercise 6.10
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State { s =>
      val (a, s2) = this.run(s)
      f(a).run(s2)
    }
}

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  // Exercise 6.10
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  // Exercise 6.10
  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] =
    sas.foldRight(unit[S, List[A]](Nil))((f, acc) => f.map2(acc)(_ :: _))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def get[S]: State[S, S] = State(s => (s, s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  // Exercise 6.11

  def update(i: Input): Machine => Machine = (s: Machine) =>
    (i, s) match {
      case (Coin, Machine(true, candies, coins)) => Machine(locked = false, candies, coins)
      case (Turn, Machine(false, candies, coins)) if candies > 0 => Machine(locked = true, candies - 1, coins + 1)
      case _ => s
    }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    for {
      // List[Inputs] -> List[StateTransitionTable(Inputs)] -> StateTransitionTable(List[Inputs])
      _ <- sequence(inputs map (modify[Machine] _ compose update))
      // Get resulting state
      s <- get
    } yield (s.coins, s.candies)

}