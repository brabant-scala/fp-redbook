package nl.hugo.redbook.ch8

import nl.hugo.redbook.ch5.Stream
import nl.hugo.redbook.ch6._
import nl.hugo.redbook.ch7._
import nl.hugo.redbook.ch7.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{ ExecutorService, Executors }

// 8.1
//val prop = forall(intList)(il => il.sum == il.reverse.sum) &&
//           forall(intList)(il => il.sum == il.foldLeft(0)(_ + _))
//           forall(intList)((l1,l2) => l1.sum + l2.sum == (l1++l2).sum)

//8.2
//
//val prop = forall(intList)(il => if (il.isEmpty) true else !il.exists(_ > il.max))

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  // Exercise 8.9
  def &&(p: Prop): Prop = ???

  // Exercise 8.9
  def ||(p: Prop): Prop = ???
}

object Prop {
  type MaxSize = Int // MaxSize parameter comes into play from exercise 8.10.
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
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeLessThan(stopExclusive - start)).map(_ + start))

  // Exercise 8.5
  def unit[A](a: => A): Gen[A] =
    Gen(State(RNG.unit(a)))

  // Exercise 8.5
  def boolean: Gen[Boolean] =
    Gen(State(RNG.boolean))

  // Exercise 8.5
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(g.sample)))
//  {
//    g.map { a =>
//      val (list, rngFinal) = (0 until n).foldRight((List.empty[A], g.sample)) { (_, acc) =>
//        val (i, rngx) = acc._2.nextInt
//        a match {
//          case v: Int    => (v +: acc._1, rngx)
//          case v: Double => (v +: acc._1, rngx)
//          case _ => ???
//        }
//      }
//      (rngFinal, list.reverse)
//    }
//  }
//{
////    Gen(State(RNG.ints(n)))
////    RNG.sequence[A]((0 to n).map(_ => Gen(State(RNG.int)))))
//    val (list, rngFinal) = (0 until n).foldRight((List.empty[A], g.sample)) { (_, acc) =>
//      val (i, rngx) = acc._2.
//      (i +: acc._1, rngx)
//    }
//    (rngFinal, RNG.sequence(list.reverse))
//  }

  // Exercise 8.7
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean.flatMap(b => if (b) g1 else g2)

  // Exercise 8.8
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] =
    Gen(State(RNG.double)).flatMap(v => if (v < g1._2 / (g1._2 + g2._2)) g1._1 else g2._1)

  // Exercise 8.12
  def listOf[A](g: Gen[A]): SGen[List[A]] = ???

  // Exercise 8.13
  def listOf1[A](g: Gen[A]): SGen[List[A]] = ???

  object ** {
    def unapply[A, B](p: (A, B)) = Some(p)
  }

}

case class Gen[+A](sample: State[RNG, A]) {
  //def map[A, B](f: A => B): Gen[B] = ???
  def map[B](f: A => B): Gen[B] = Gen(sample.map(f))

  // Exercise 8.6
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(s => f(s).sample))

  def listOfN(size: Int): Gen[List[A]] = Gen.listOfN(size, this)

  // Exercise 8.6
  def listOfN(size: Gen[Int]): Gen[List[A]] = //Gen(size.flatMap(n => listOfN(n)).sample) // size was al een Gen, dus kan korter
    size flatMap (l => this.listOfN(l))

  // Exercise 8.10
  def unsized: SGen[A] = SGen(_ => this)

  def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] = ???

  def **[B](g: Gen[B]): Gen[(A, B)] = (this map2 g)((_, _))

}

case class SGen[+A](forSize: Int => Gen[A]) {
  // Exercise 8.11
  def apply(n: Int): Gen[A] = ???

  // Exercise 8.11
  def map[B](f: A => B): SGen[B] = ???

  // Exercise 8.11
  def flatMap[B](f: A => SGen[B]): SGen[B] = ???

  // Exercise 8.11
  def **[B](s2: SGen[B]): SGen[(A, B)] = ???
}
