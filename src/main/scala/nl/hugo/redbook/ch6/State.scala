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

  def toDouble(x: Int): Double = x.toDouble / (Int.MaxValue.toDouble + 1)

  // Exercise 6.01
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, rng2) = int(rng)
    (if (i == Int.MinValue) 0 else Math.abs(i), rng2)
  }

  // Exercise 6.02
  def double(rng: RNG): (Double, RNG) = {
    val (d, rng2) = nonNegativeInt(rng)
    (toDouble(d), rng2)
  }

  // Exercise 6.03
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng2) = int(rng)
    val (d, rng3) = double(rng2)
    ((i,d), rng3)
  }

  // Exercise 6.03
  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, rng2) = double(rng)
    val (i, rng3) = int(rng2)
    ((d,i), rng3)
  }

  // Exercise 6.03
  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1,d2,d3), rng4)
  }

  // Exercise 6.04
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    val (list, rngFinal) = (0 until count).foldRight((List.empty[Int], rng)) { (_, acc) =>
      val (i, rngx) = acc._2.nextInt
      (i +: acc._1, rngx)
    }
    (list.reverse, rngFinal)
  }

  // Exercise 6.05
  def doubleViaMap: Rand[Double] = map(nonNegativeInt)(toDouble)
  def nonNegativeIntViaMap(rng: RNG): (Int, RNG) = map(int)(x => if (x == Int.MinValue) 0 else Math.abs(x))(rng)

  // Exercise 6.06
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a,b), rng3)
    }
  }

  def both[A,B](ra:Rand[A], rb: Rand[B]): Rand[(A,B)] = map2(ra, rb)((_, _))
  val randIntDouble: Rand[(Int, Double)] = both(int, double)
  val randDoubleInt: Rand[(Double, Int)] = both(double, int)
  // added extra tests to 6.03 to verify the results using this approach

  // Exercise 6.07
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = { rng =>
    fs.foldRight((List.empty[A], rng))((ra, acc) => {
      val (x, rngx) = ra(acc._2)
      (x +: acc._1, rngx)
    })
  }


  // Exercise 6.07
  def intsViaSequence(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

  // Exercise 6.08
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = { rng =>
    val (a, rng2) = f(rng)
    g(a)(rng2)
  }

  // Exercise 6.08
  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt){ i =>
    val mod = i % n
    if (i + (n-1) - mod >= 0) (mod, _) else nonNegativeLessThan(n)
  }

  // Exercise 6.09
  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => (f(a),_))

  // Exercise 6.09
  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

  def boolean: Rand[Boolean] = map(int)(_ % 2 == 0)
}

case class State[S, +A](run: S => (A, S)) {
  // Exercise 6.10
  def map[B](f: A => B): State[S, B] =
    State(s1 => {
      val (a,s2) = run(s1)
      (f(a),s2)
    })

  // Exercise 6.10
  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    State(s1 => {
      val (a, s2) = run(s1)
      val (b, s3) = sb.run(s2)
      (f(a,b), s3)
    })

  // Exercise 6.10
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s1 => {
      val (a, s2) = run(s1)
      f(a).run(s2)
    })
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
    State(s1 => {
      sas.foldRight((List.empty[A], s1))((s, acc) => {
        val (x, sn) = s.run(acc._2)
        (x +: acc._1, sn)
      })
    })

//  def get[S]: State[S,S] = State(s => (s,s))
//
//  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
//
//  def modify[S](f: S => S): State[S, Unit] = for {
//    s <- get
//    _ <- set(f(s))
//  } yield ()

  // Exercise 6.11
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    State(s0 => {
      inputs.foldLeft(((s0.coins, s0.candies), s0)){ (acc, input) =>
        (input, acc) match {
          case (Coin, (_, Machine(locked, candies, coins))) if locked && candies > 0 =>
            ((coins + 1, candies), Machine(false, candies, coins + 1))
          case (Turn, (_, Machine(locked, candies, coins))) if !locked && candies > 0 =>
            ((coins, candies - 1), Machine(true, candies - 1, coins))
          case _ =>
            (acc._1, acc._2)
        }
      }
    })
  }
}
