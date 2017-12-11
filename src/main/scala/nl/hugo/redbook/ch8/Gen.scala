package nl.hugo.redbook.ch8

import nl.hugo.redbook.ch5.Stream
import nl.hugo.redbook.ch6._
import nl.hugo.redbook.ch7._
import nl.hugo.redbook.ch7.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{ ExecutorService, Executors }

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {

  // Exercise 8.9
  def &&(p: Prop): Prop =
    lazy_&&(p)

  // exercise 8.9 - lazy
  def lazy_&&(p: Prop): Prop =
    Prop { (m, n, r) =>
      run(m, n, r) match {
        case Passed | Proved => p.run(m, n, r)
        case falsified => falsified
      }
    }

  // exercise 8.9 - eager
  def eager_&&(p: Prop): Prop =
    Prop { (m, n, r) =>
      (run(m, n, r), p.run(m, n, r)) match {
        case (Proved, Proved) => Proved
        case (Proved | Passed, Proved | Passed) => Passed
        case (f, Proved | Passed) => f
        case (Proved | Passed, f) => f
        case (Falsified(f1, s1), Falsified(f2, s2)) =>
          Falsified(s"$f1\n$f2", s1 + s2)
      }
    }

  // Exercise 8.9
  def ||(p: Prop): Prop =
    lazy_||(p)

  // exercise 8.9 - lazy
  def lazy_||(p: Prop): Prop =
    Prop { (m, n, r) =>
      run(m, n, r) match {
        case Falsified(f1, s1) =>
          p.run(m, n, r) match {
            case Falsified(f2, s2) => Falsified(s"$f1\n$f2", s1 + s2)
            case other => other
          }
        case other => other
      }
    }

  // exercise 8.9 - eager
  def eager_||(p: Prop): Prop =
    Prop { (m, n, r) =>
      (run(m, n, r), p.run(m, n, r)) match {
        case (_, Proved) | (Proved, _) => Proved
        case (_, Passed) | (Passed, _) => Passed
        case (Falsified(f1, s1), Falsified(f2, s2)) =>
          Falsified(s"$f1\n$f2", s1 + s2)
      }
    }
}

object Prop {
  type MaxSize = Int // MaxSize parameter comes into play from exersize 8.10.
  type TestCases = Int
  type FailedCase = String
  type SuccessCount = Int

  sealed trait Result {
    def isFalsified: Boolean
  }
  case object Passed extends Result {
    def isFalsified = false
  }
  case class Falsified(
    failure: FailedCase,
    successes: SuccessCount
  ) extends Result {
    def isFalsified = true
  }
  case object Proved extends Result {
    def isFalsified = false
  }

  /* Produce an infinite random stream from a `Gen` and a starting `RNG`. */
  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (m, n, rng) =>
      randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
        case (a, i) => try {
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch {
          case e: Exception => Falsified(buildMsg(a, e), i)
        }
      }.find(_.isFalsified).getOrElse(Passed)
  }

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n + (max - 1)) / max
      val props: Stream[Prop] = Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop = props.map(p => Prop { (max, _, rng) =>
        p.run(max, casesPerSize, rng)
      }).toList.reduce(_ && _)
      prop.run(max, n, rng)
  }

  def check(p: => Boolean): Prop = Prop { (_, _, _) =>
    if (p) Passed else Falsified("()", 0) // TODO: Passed or Proved?
  }

  // section 8.4.2
  def equal[A](p1: Par[A], p2: Par[A]): Par[Boolean] =
    Par.map2(p1, p2)(_ == _)

  val S: Gen[ExecutorService] = Gen.weighted(
    Gen.choose(1, 4).map(Executors.newFixedThreadPool) -> .75,
    Gen.unit(Executors.newCachedThreadPool) -> .25
  )

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S.map2(g)((_, _))) { case (s, a) => Par.run(s)(f(a)) } // TODO: get is blocking!

  def run(
    p: Prop,
    maxSize: Int = 100,
    testCases: Int = 100,
    rng: RNG = RNG.Simple(System.currentTimeMillis)
  ): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Proved =>
        println(s"+ OK, proved property.")
    }
}

object Gen {
  // Exercise 8.4
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeLessThan(stopExclusive - start)).map(_ + start))

  // Exercise 8.5
  def unit[A](a: => A): Gen[A] =
    Gen(State.unit(a))

  // Exercise 8.5
  def boolean: Gen[Boolean] =
    boolean_2

  def boolean_1: Gen[Boolean] =
    Gen(State(RNG.int).map(_ % 2 == 0))

  def boolean_2: Gen[Boolean] =
    int.map(_ % 2 == 0)

  def boolean_3: Gen[Boolean] =
    of(RNG.boolean)

  def boolean_4: Gen[Boolean] =
    choose(0, 2).map(_ == 0)

  def of[A](f: RNG.Rand[A]): Gen[A] =
    Gen(State(f))

  def int: Gen[Int] =
    of(RNG.int)

  def double: Gen[Double] =
    of(RNG.double)

  // Exercise 8.5
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    listOfN_1(n, g)

  def listOfN_1[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  def listOfN_2[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(g.sample.map2(listOfN(n - 1, g).sample)(_ :: _))

  // Exercise 8.7
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap { b =>
      if (b) g1 else g2
    }

  // Exercise 8.8
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] =
    double.flatMap { d =>
      if (d < g1._2 / (g1._2 + g2._2)) g1._1 else g2._1
    }

  // Exercise 8.12
  def listOf[A](g: Gen[A]): SGen[List[A]] =
    listOf_1(g)

  def listOf_1[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => Gen.listOfN(n, g))

  def listOf_2[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => g.listOfN(n))

  // Exercise 8.13
  def listOf1[A](g: Gen[A]): SGen[List[A]] =
    listOf1_1(g)

  def listOf1_1[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => listOfN(n max 1, g))

  def listOf1_2[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => listOfN(math.abs(n) + 1, g))

  object ** {
    def unapply[A, B](p: (A, B)) = Some(p)
  }
}

case class Gen[+A](sample: State[RNG, A]) {
  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f))

  // Exercise 8.6
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  def listOfN(size: Int): Gen[List[A]] =
    Gen.listOfN(size, this)

  // Exercise 8.6
  def listOfN(size: Gen[Int]): Gen[List[A]] =
    listOfN(size)

  def listOfN_1(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(n => Gen.listOfN(n, this))

  def listOfN_2(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(n => listOfN(n))

  def listOfN_3(size: Gen[Int]): Gen[List[A]] =
    for {
      s <- size
      l <- listOfN(s)
    } yield l

  // Exercise 8.10
  def unsized: SGen[A] =
    SGen(_ => this)

  def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] =
    Gen(sample.map2(g.sample)(f))

  def **[B](g: Gen[B]): Gen[(A, B)] =
    this.map2(g)((_, _))
}

case class SGen[+A](forSize: Int => Gen[A]) {
  // Exercise 8.11
  def apply(n: Int): Gen[A] =
    forSize(n)

  // Exercise 8.11
  def map[B](f: A => B): SGen[B] =
    SGen(n => forSize(n).map(f))

  // Exercise 8.11
  def flatMap[B](f: A => SGen[B]): SGen[B] =
    SGen(n => forSize(n).flatMap(a => f(a).forSize(n)))

  // Exercise 8.11
  def **[B](s2: SGen[B]): SGen[(A, B)] =
    SGen(n => forSize(n) ** s2(n))
}
