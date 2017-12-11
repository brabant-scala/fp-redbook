package nl.hugo.redbook.ch12

import nl.hugo.redbook.ch10._
import nl.hugo.redbook.ch11.Functor
import nl.hugo.redbook.ch12.StateUtil._
import nl.hugo.redbook.ch6._

import scala.language.{ higherKinds, implicitConversions }

trait Applicative[F[_]] extends Functor[F] {

  // Exercise 12.02
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = ???

  // Exercise 12.02
  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] = ???

  def unit[A](a: => A): F[A]

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)

  // Exercise 12.01
  def sequence[A](fas: List[F[A]]): F[List[A]] = ???

  // Exercise 12.01
  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] = ???

  // Exersice 12.01
  def product[A,B](fa: F[A], fb: F[B]): F[(A,B)] = ???

  // Exercise 12.01
  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = ???

  // Exercise 12.03
  def map3[A, B, C, D](
    fa: F[A],
    fb: F[B],
    fc: F[C]
  )(f: (A, B, C) => D): F[D] = ???

  // Exercise 12.03
  def map4[A, B, C, D, E](
    fa: F[A],
    fb: F[B],
    fc: F[C],
    fd: F[D]
  )(f: (A, B, C, D) => E): F[E] = ???

  def factor[A, B](fa: F[A], fb: F[B]): F[(A, B)] = ???

  // Exercise 12.08
  def product[G[_]](G: Applicative[G]): Applicative[({ type f[x] = (F[x], G[x]) })#f] = ???

  // Exercise 12.09
  def compose[G[_]](G: Applicative[G]): Applicative[({ type f[x] = F[G[x]] })#f] = ???

  // Exercise 12.12
  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] = ???
}

case class Tree[+A](head: A, tail: List[Tree[A]])

trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B] = join(map(ma)(f))

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  override def apply[A, B](mf: F[A => B])(ma: F[A]): F[B] =
    flatMap(mf)(f => map(ma)(a => f(a)))
}

object Monad {
  // Exercise 12.05
  def eitherMonad[E]: Monad[({ type f[x] = Either[E, x] })#f] = ???

  def stateMonad[S] = new Monad[({ type f[x] = State[S, x] })#f] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    override def flatMap[A, B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }

  // Exercise 12.20
  def composeM[F[_], N[_]](implicit F: Monad[F], N: Monad[N], T: Traverse[N]): Monad[({ type f[x] = F[N[x]] })#f] = ???
}

sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E])
  extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]

object Applicative {

  val streamApplicative = new Applicative[Stream] {

    def unit[A](a: => A): Stream[A] =
      Stream.continually(a) // The infinite, constant stream

    override def map2[A, B, C](a: Stream[A], b: Stream[B])( // Combine elements pointwise
      f: (A, B) => C
    ): Stream[C] =
      a zip b map f.tupled
  }

  // Exercise 12.06
  def validationApplicative[E]: Applicative[({ type f[x] = Validation[E, x] })#f] = ???

  type Const[A, B] = A

  implicit def monoidApplicative[M](M: Monoid[M]) =
    new Applicative[({ type f[x] = Const[M, x] })#f] {
      def unit[A](a: => A): M = M.zero
      override def apply[A, B](m1: M)(m2: M): M = M.op(m1, m2)
    }
}

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
    sequence(map(fa)(f))
  def sequence[G[_]: Applicative, A](fma: F[G[A]]): G[F[A]] =
    traverse(fma)(ma => ma)

  // Exercise 12.14
  def map[A, B](fa: F[A])(f: A => B): F[B] = ???

  import Applicative._

  override def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    traverse[({ type f[x] = Const[B, x] })#f, A, Nothing](
      as
    )(f)(monoidApplicative(mb))

  def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({ type f[x] = State[S, x] })#f, A, B](fa)(f)(Monad.stateMonad)

  def mapAccum[S, A, B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)((a: A) => (for {
      s1 <- get[S]
      (b, s2) = f(a, s1)
      _ <- set(s2)
    } yield b)).run(s)

  override def toList[A](fa: F[A]): List[A] =
    mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

  // Exercise 12.16
  def reverse[A](fa: F[A]): F[A] = ???

  // Exercise 12.17
  override def foldLeft[A, B](fa: F[A])(z: B)(f: (B, A) => B): B = ???

  // Exercise 12.18
  def fuse[G[_], H[_], A, B](fa: F[A])(f: A => G[B], g: A => H[B])(implicit G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) = ???

  // Exericse 12.19
  def compose[G[_]](implicit G: Traverse[G]): Traverse[({ type f[x] = F[G[x]] })#f] = ???
}

object Traverse {
  // Exercise 12.13
  val listTraverse = ???

  // Exercise 12.13
  val optionTraverse = ???

  // Exercise 12.13
  val treeTraverse = ???
}

// The `get` and `set` functions on `State` are used above,
// but aren't in the `exercises` subproject, so we include
// them here
object StateUtil {

  def get[S]: State[S, S] =
    State(s => (s, s))

  def set[S](s: S): State[S, Unit] =
    State(_ => ((), s))
}