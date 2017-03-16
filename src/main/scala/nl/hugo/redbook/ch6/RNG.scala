package nl.hugo.redbook.ch6

trait RNG {
  def nextInt: (Int, RNG)
}


object RNG{
  type Rand[+A] = RNG => (A, RNG)

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = {
    flatMap(s) { a => rng2 =>
      (f(a), rng2)
    }
  }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, rngA) = f(rng)
    g(a)(rngA)
  }

  def mapV1[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra) { a =>
        flatMap(rb) { b => rng3 => (f(a, b), rng3)
        }
    }
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) { i =>
      rng =>
        val mod = i % n
        if (i + (n - 1) - mod >= 0) (mod, rng) else nonNegativeLessThan(n)(rng)
    }
  }

  def map2V1[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rnga) = ra(rng)
    val (b, rngb) = rb(rnga)
    (f(a,b), rngb)
  }


  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {

    def recurInts(count: Int, ints: List[Int])(rng2: RNG) : (List[Int], RNG)={
      count match{
        case count0 if count0 <= 0 => (ints, rng2)
        case _ => {
          val (integer, rng3) = rng2.nextInt
          recurInts(count -1, ints :+ integer   )(rng3)
        }
      }
    }
    recurInts(count, List.empty[Int])(rng)
  }


  def double3(rng: RNG) = {
    val (double1, rng1) = double(rng)
    val (double2, rng2) = double(rng1)
    val (double3, rng3) = double(rng2)

    ((double1, double2, double3), rng3)
  }

  def doubleInt(rng: RNG) = {
    val ((integer, double), rng2) = intDouble(rng)
    ((double, integer), rng2)
  }

  def intDouble(rng: RNG) = {
    val (integerVal, intRng) = rng.nextInt
    val (doubleVal, doubleRng) = double(intRng)
    ((integerVal, doubleVal), doubleRng)
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (randomNr, randomizer) = rng.nextInt
    if (randomNr == Integer.MIN_VALUE) {
      (0, randomizer)
    } else {
      (math.abs(randomNr), randomizer)
    }
  }
  def double: Rand[Double] = {
    RNG.map(_.nextInt) { randomNr:Int =>
      val randomDouble = randomNr.toDouble / (Int.MaxValue.toDouble + 1) match {
        //randomNr.toDouble/Int.MaxValue match{
        case 1 => 0
        case other => other
      }
      (randomDouble)
    }
  }

  def doubleV1(rng: RNG): (Double, RNG) = {
    val (randomNr, randomizer) = rng.nextInt
    val randomDouble = randomNr.toDouble / (Int.MaxValue.toDouble + 1) match { //randomNr.toDouble/Int.MaxValue match{
      case 1 => 0
      case other => other
    }
    (randomDouble, randomizer)
  }


}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }


}

