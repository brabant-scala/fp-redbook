package nl.hugo.redbook.ch8

import nl.hugo.redbook.ch5.Stream
import nl.hugo.redbook.ch6._
import nl.hugo.redbook.ch7._
import nl.hugo.redbook.ch7.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{ExecutorService, Executors}

import nl.hugo.redbook.ch6.State.Rand

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {self =>
  // Exercise 8.9
  def &&(p: Prop): Prop = Prop(
    (maxSize, testCases, rng) => {
      self.run(maxSize, testCases, rng) match {
        case Falsified(msg, cnt) => Falsified(msg, cnt) //Falsified(s"Left($msg)", cnt)
        case x =>
          (x, p.run(maxSize, testCases, rng)) match {
            case (_,Falsified(msg, cnt)) => Falsified(msg, cnt) //Falsified(s"Right($msg)", cnt)
            case (Proved, Proved) => Proved
            case (_,_) => Passed
          }
      }
    }
  )

  // Exercise 8.9
  def ||(p: Prop): Prop = Prop(
    (maxSize, testCases, rng) => {
      self.run(maxSize, testCases, rng) match {
        case Falsified(_,_) =>
          p.run(maxSize, testCases, rng) match {
            case Falsified(msg, cnt) => Falsified(msg, cnt)//Falsified(s"Left($msg1), Right($msg2)", cnt1 min cnt2)
            case x => x
          }
        case x => x
      }
    }
  )
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

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = forAll(g(_))(f)

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
    if (p) Passed else Falsified("()", 0)
  }

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
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen(State(RNG.nonNegativeInt).map(x => start + x %(stopExclusive - start)))
  }

  // Exercise 8.5
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  // Exercise 8.5
  def boolean: Gen[Boolean] = choose(0, 2).map(_ == 0)

  // Exercise 8.5
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    if (n <= 0) {
      unit(Nil)
    } else {
      Gen(g.sample.map2(listOfN(n-1, g).sample)(_ :: _))
    }
  }

  // Exercise 8.7
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = {
    for {
      useFirst <- boolean
      value <- if (useFirst) g1 else g2
    } yield value
  }

  // Exercise 8.8
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    for {
      probability <- Gen(State(RNG.double)).map((d: Double) => d*(g1._2 + g2._2))
      value <- if (probability < g1._2) g1._1 else g2._1
    } yield value
  }

  // Exercise 8.12
  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(n => Gen.listOfN(n, g))

  // Exercise 8.13
  def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen(
    n => {
      for {
        a <- g
        list <- listOfN(n, g)
      } yield a :: list
    }
  )

  object ** {
    def unapply[A, B](p: (A, B)) = Some(p)
  }

}

case class Gen[+A](sample: State[RNG, A]) {
  //def map[A, B](f: A => B): Gen[B] = ???
  def map[B](f: A => B): Gen[B] = Gen(sample.map(f))

  // Exercise 8.6
  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    Gen(sample.flatMap(a => f(a).sample))
  }

  def listOfN(size: Int): Gen[List[A]] = Gen.listOfN(size, this)

  // Exercise 8.6
  def listOfN(size: Gen[Int]): Gen[List[A]] = {
    for {
      s <- size
      l <- listOfN(s)
    } yield l
  }

  // Exercise 8.10
  def unsized: SGen[A] = SGen(_ => this)

  def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] = {
    for {
      x <- this
      y <- g
    } yield f(x,y)
  }

  def **[B](g: Gen[B]): Gen[(A, B)] = (this map2 g)((_, _))

}

case class SGen[+A](forSize: Int => Gen[A]) {
  // Exercise 8.11
  def apply(n: Int): Gen[A] = forSize(n)

  // Exercise 8.11
  def map[B](f: A => B): SGen[B] = SGen(size => forSize(size).map(f))

  // Exercise 8.11
  def flatMap[B](f: A => SGen[B]): SGen[B] = SGen(size => forSize(size).flatMap(a => f(a)(size)))

  def map2[B, C](g: SGen[B])(f: (A, B) => C): SGen[C] = {
    for {
      x <- this
      y <- g
    } yield f(x,y)
  }

  // Exercise 8.11
  def **[B](s2: SGen[B]): SGen[(A, B)] = (this map2 s2)((_,_))
}
