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
    val (n, next) = rng.nextInt
    (n & Int.MaxValue, next) // reset sign bit
  }

  // Exercise 6.02
  def double(rng: RNG): (Double, RNG) = {
    val (n, next) = nonNegativeInt(rng)
    (-n / Int.MinValue.toDouble, next) // alternative to n / (Int.MaxValue.toDouble + 1)
  }

  // Exercise 6.03
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (n, r1) = nonNegativeInt(rng)
    val (d, r2) = double(r1)
    ((n, d), r2)
  }

  // Exercise 6.03
  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((n, d), r) = intDouble(rng)
    ((d, n), r)
  }

  // Exercise 6.03
  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  // Exercise 6.04
  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    ints_1(count)(rng)

  // ints - imperative
  def ints_1(count: Int)(rng: RNG): (List[Int], RNG) = {
    var is: List[Int] = Nil
    var r = rng
    for (_ <- 1 to count) {
      val (n, next) = nonNegativeInt(r)
      is = n :: is
      r = next
    }
    (is, r)
  }

  // ints - (non tail-) recursive
  def ints_2(count: Int)(rng: RNG): (List[Int], RNG) =
    if (count <= 0) (Nil, rng)
    else {
      val (head, r1) = nonNegativeInt(rng)
      val (tail, r2) = ints_2(count - 1)(r1)
      (head :: tail, r2)
    }

  // ints - tail-recursive
  def ints_3(count: Int)(rng: RNG): (List[Int], RNG) = {

    def go(c: Int, acc: List[Int])(r: RNG): (List[Int], RNG) =
      if (c <= 0) (acc, r)
      else {
        val (i, r1) = nonNegativeInt(r)
        go(c - 1, i :: acc)(r1)
      }

    go(count, Nil)(rng)
  }

  // Exercise 6.05
  def doubleViaMap: Rand[Double] =
    map(nonNegativeInt)(i => -i / Int.MinValue.toDouble)

  // Exercise 6.06
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }

  // Exercise 6.07
  def sequence[A](l: List[Rand[A]]): Rand[List[A]] =
    l.foldRight(unit[List[A]](Nil)) { (ra, ras) =>
      map2(ra, ras)(_ :: _)
    }

  // Exercise 6.07
  def intsViaSequence(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  // Exercise 6.08
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }

  // Exercise 6.08
  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0)
        unit(mod)
      else
        nonNegativeLessThan(n)
    }

  // Exercise 6.09
  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  // Exercise 6.09
  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))
}

case class State[S, +A](run: S => (A, S)) {
  import State._

  // Exercise 6.10
  def map[B](f: A => B): State[S, B] =
    map_2(f)

  // map - literal implementation
  def map_1[B](f: A => B): State[S, B] =
    State { s =>
      val (a, s2) = run(s)
      (f(a), s2)
    }

  // map - based on flatMap
  def map_2[B](f: A => B): State[S, B] =
    flatMap(a => unit[S, B](f(a)))

  // Exercise 6.10
  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    map2_2(sb)(f)

  // map2 - literal implementation
  def map2_1[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    State { s =>
      val (a, s2) = run(s)
      val (b, s3) = sb.run(s2)
      (f(a, b), s3)
    }

  // map2 - based on flatMap
  def map2_2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  // Exercise 6.10
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State { s =>
      val (a, s2) = run(s)
      f(a).run(s2)
    }
}

object State {
  type Rand[A] = State[RNG, A]

  // Exercise 6.10
  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  // Exercise 6.10
  def sequence[S, A](l: List[State[S, A]]): State[S, List[A]] =
    l.foldRight(unit[S, List[A]](Nil)) { (sa, sas) =>
      sa.map2(sas)(_ :: _)
    }

  // Exercise 6.10 - map2 companion variant
  def map2[S, A, B, C](sa: State[S, A], sb: State[S, B])(f: (A, B) => C): State[S, C] =
    sa.map2(sb)(f)

  // From section 6.6
  def get[S]: State[S, S] =
    State(s => (s, s))

  // From section 6.6
  def set[S](s: S): State[S, Unit] =
    State(_ => ((), s))

  // From section 6.6
  def modify[S](f: S => S): State[S, Unit] =
    for {
      s <- get
      _ <- set(f(s))
    } yield ()

  // Exercise 6.11
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    Machine.simulate_2(inputs)
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int) {

  // Exercise 6.11
  def handle(input: Input): Machine =
    input match {
      case Coin if locked && candies > 0 =>
        copy(locked = false, coins = coins + 1)
      case Turn if !locked && candies > 0 =>
        copy(locked = true, candies = candies - 1)
      case _ =>
        this
    }
}

object Machine {
  import State._

  // Exercise 6.11 - using foldLeft
  def simulate_1(inputs: List[Input]): State[Machine, (Int, Int)] =
    State { machine =>
      val m2 = inputs.foldLeft(machine) { (m, i) => m.handle(i) }
      ((m2.coins, m2.candies), m2)
    }

  // Exercise 6.11 - using sequence
  def simulate_2(inputs: List[Input]): State[Machine, (Int, Int)] = {
    val ss: List[State[Machine, Unit]] = inputs.map(i => modify[Machine](_.handle(i)))
    val s: State[Machine, List[Unit]] = sequence(ss)
    val sm: State[Machine, Machine] = s.flatMap(_ => get)
    sm.map(m => (m.coins, m.candies))
  }

  // Exercise 6.11 - using sequence and for-comprehension
  def simulate_3(inputs: List[Input]): State[Machine, (Int, Int)] =
    for {
      _ <- sequence(inputs.map(i => modify[Machine](_.handle(i))))
      m <- get
    } yield (m.coins, m.candies)
}
