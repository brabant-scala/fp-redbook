package nl.hugo.redbook.ch11

import nl.hugo.redbook.ch7.Par
import nl.hugo.redbook.ch7.Par._
import nl.hugo.redbook.ch4.Option
import nl.hugo.redbook.ch4.Some
import nl.hugo.redbook.ch5.Stream
import nl.hugo.redbook.ch6.State
import nl.hugo.redbook.ch8.Gen
import nl.hugo.redbook.ch9.Parsers

import scala.{ Either => _, Option => _, Some => _, Stream => _ }
import scala.language.higherKinds
import scala.language.reflectiveCalls

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]

  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa) => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }
}

object Functor {
  val listFunctor = new Functor[List] {
    def map[A, B](as: List[A])(f: A => B): List[B] = as map f
  }
}

trait Monad[M[_]] extends Functor[M] {
  def unit[A](a: => A): M[A]
  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

  def map[A, B](ma: M[A])(f: A => B): M[B] =
    flatMap(ma)(a => unit(f(a)))
  def map2[A, B, C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  // Exercise 11.3
  def sequence[A](lma: List[M[A]]): M[List[A]] =
    lma.foldRight(unit(List.empty[A]))((f, acc) => map2(f, acc)(_ :: _))

  // Exercise 11.3
  def traverse[A, B](la: List[A])(f: A => M[B]): M[List[B]] =
    la.foldRight(unit(List.empty[B]))((v, acc) => map2(f(v), acc)(_ :: _))

  // Exercise 11.4
  def replicateM[A](n: Int, ma: M[A]): M[List[A]] =
    sequence(List.fill(n)(ma))

  // Exercise 11.6
  def filterM[A](ms: List[A])(f: A => M[Boolean]): M[List[A]] =
    ms.foldRight(unit(List.empty[A]))((v, acc) =>
      flatMap(f(v))(
        (b: Boolean) =>
          if (b)
            map2(unit(v), acc)(_ :: _)
          else
            acc
      ))

  // Exercise 11.7
  def compose[A, B, C](f: A => M[B], g: B => M[C]): A => M[C] =
    (a: A) => flatMap(f(a))(b => g(b))

  // Exercise 11.8
  // Implement in terms of `compose`:
  def _flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] =
    compose((_: Unit) => ma, f)(())

  // Exercise 11.12
  def join[A](mma: M[M[A]]): M[A] =
    flatMap(mma)(identity)

  // Exercise 11.13
  // Implement in terms of `join`:
  def __flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] =
    join(map(ma)(f))
}

case class Reader[R, A](run: R => A)

object Monad {
  val genMonad: Monad[Gen] = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)
    override def flatMap[A, B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
      ma flatMap f
  }

  // Exercise 11.1
  val parMonad: Monad[Par] = new Monad[Par] {
    def unit[A](a: => A): Par[A] = Par.unit(a)
    def flatMap[A, B](ma: Par[A])(f: A => Par[B]): Par[B] =
      Par.flatMap(ma)(f)
  }

  // Exercise 11.1
  def parserMonad[P[+_]](p: Parsers[P]): Monad[P] = new Monad[P] {
    def unit[A](a: => A): P[A] = p.succeed(a)
    def flatMap[A, B](ma: P[A])(f: A => P[B]): P[B] =
      p.flatMap(ma)(f)
  }

  // Exercise 11.1
  lazy val optionMonad: Monad[Option] = new Monad[Option] {
    def unit[A](a: => A) = Some(a)
    def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] =
      ma.flatMap(f)
  }

  // Exercise 11.1
  lazy val streamMonad: Monad[Stream] = new Monad[Stream] {
    def unit[A](a: => A): Stream[A] = Stream(a)
    def flatMap[A, B](ma: Stream[A])(f: A => Stream[B]): Stream[B] =
      ma.flatMap(f)
  }

  // Exercise 11.1
  lazy val listMonad: Monad[List] = new Monad[List] {
    def unit[A](a: => A): List[A] = List(a)
    def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] =
      ma flatMap f
  }

  class StateMonadS[S] {
    type StateS[A] = State[S, A]

    val monad = new Monad[StateS] {
      def unit[A](a: => A): StateS[A] = State.unit[S, A](a)
      def flatMap[A, B](ma: StateS[A])(f: A => StateS[B]): StateS[B] =
        ma.flatMap(f)
    }
  }

  // Exercise 11.2
  def stateMonad[S] = new StateMonadS[S].monad

  // Exercise 11.17
  lazy val idMonad: Monad[Id] = new Monad[Id] {
    def unit[A](a: => A): Id[A] = Id(a)
    def flatMap[A, B](ma: Id[A])(f: A => Id[B]): Id[B] =
      ma.flatMap(f)
  }

  // Exercise 11.20
  def readerMonad[R] = new Monad[({ type f[x] = Reader[R, x] })#f] {
    def unit[A](a: => A): Reader[R, A] = Reader(_ => a)
    override def flatMap[A, B](st: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] =
      Reader(r => {
        val thisResult = st.run(r)
        f(thisResult).run(r)
      })
  }

  def getR[R]: Reader[R, R] = Reader(r => r)
}

case class Id[A](value: A) {
  // Exercise 11.17
  def map[B](f: A => B): Id[B] = Id(f(value))
  // Exercise 11.17
  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}
