package nl.hugo.redbook.ch12

import language.higherKinds
import language.implicitConversions
import scala.util.Try
import nl.hugo.redbook.ch6.State
import nl.hugo.redbook.ch10.{ Foldable, Monoid }
import nl.hugo.redbook.ch11.Functor

// Listing 12.1
trait Applicative[F[_]] extends Functor[F] {

  // Listing 12.1
  def unit[A](a: => A): F[A]

  // Listing 12.1
  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List.empty[B]))((a, fbs) => map2(f(a), fbs)(_ :: _))

  // Exercise 12.1
  def sequence[A](fas: List[F[A]]): F[List[A]] =
    ???

  // Exercise 12.1
  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    ???

  // Exercise 12.2
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    ???

  // Exercise 12.2
  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    ???

  // Exercise 12.2
  def map[A, B](fa: F[A])(f: A => B): F[B] =
    ???

  // Exercise 12.8
  def product[G[_]](g: Applicative[G]): Applicative[({ type t[x] = (F[x], G[x]) })#t] =
    ???

  // Exercise 12.9
  def compose[G[_]](g: Applicative[G]): Applicative[({ type t[x] = F[G[x]] })#t] =
    ???

  // Exercise 12.12
  def sequenceMap[K, V](m: Map[K, F[V]]): F[Map[K, V]] =
    ???
}

object Applicative {

  // Section 12.4.1
  val streamApplicative = new Applicative[Stream] {
    def unit[A](a: => A): Stream[A] =
      Stream.continually(a)
    override def map2[A, B, C](a: Stream[A], b: Stream[B])(f: (A, B) => C): Stream[C] =
      a.zip(b).map(f.tupled)
  }

  // Exercise 12.6
  def validationApplicative[E]: Applicative[({ type t[x] = Validation[E, x] })#t] =
    ???

  // Listing 12.8
  type Const[A, B] = A

  // Listing 12.8
  implicit def monoidApplicative[M](m: Monoid[M]) =
    new Applicative[({ type f[x] = Const[M, x] })#f] {
      def unit[A](a: => A): M = m.zero
      override def apply[A, B](m1: M)(m2: M): M = m.op(m1, m2)
    }
}

// Listing 12.1
trait Monad[F[_]] extends Applicative[F] {

  // Listing 12.2
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] =
    join(map(fa)(f))

  // Listing 12.2
  def join[A](ffa: F[F[A]]): F[A] =
    flatMap(ffa)(identity)

  // Listing 12.2
  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  // Listing 12.2
  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => unit(f(a)))

  // Listing 12.2
  override def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a, b)))

  // Exercise 12.11
  def compose[G[_]](g: Monad[G]): Monad[({ type t[x] = F[G[x]] })#t] =
    Try(???).getOrElse(???) // impossible!
}

object Monad {

  // Section 11.5.2
  def stateMonad[S] = new Monad[({ type t[x] = State[S, x] })#t] {
    def unit[A](a: => A): State[S, A] =
      State(s => (a, s))
    override def flatMap[A, B](s: State[S, A])(f: A => State[S, B]): State[S, B] =
      s.flatMap(f)
  }

  // Exercise 12.5
  def eitherMonad[E]: Monad[({ type t[x] = Either[E, x] })#t] =
    ???

  // Exercise 12.20
  def composeM[F[_], G[_]](implicit f: Monad[F], g: Monad[G], t: Traverse[G]): Monad[({ type t[x] = F[G[x]] })#t] =
    ???
}

// Section 12.6
trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  import Applicative.{ Const, monoidApplicative }

  // Section 12.6
  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
    sequence(map(fa)(f))

  // Section 12.6
  def sequence[G[_]: Applicative, A](fga: F[G[A]]): G[F[A]] =
    traverse(fga)(identity)

  // Exercise 12.14
  def map[A, B](fa: F[A])(f: A => B): F[B] =
    ???

  // Listing 12.8
  override def foldMap[A, B](fa: F[A])(f: A => B)(mb: Monoid[B]): B =
    traverse[({ type t[x] = Const[B, x] })#t, A, Nothing](fa)(f)(monoidApplicative(mb))

  // Section 12.7.2
  def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({ type t[x] = State[S, x] })#t, A, B](fa)(f)(Monad.stateMonad)

  // Listing 12.11
  def mapAccum[S, A, B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)((a: A) => (for {
      s1 <- State.get[S]
      (b, s2) = f(a, s1)
      _ <- State.set(s2)
    } yield b)).run(s)

  // Listing 12.11
  override def toList[A](fa: F[A]): List[A] =
    mapAccum(fa, List.empty[A])((a, s) => ((), a :: s))._2.reverse

  // Listing 12.11
  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

  // Exercise 12.16
  def reverse[A](fa: F[A]): F[A] =
    ???

  // Exercise 12.17
  override def foldLeft[A, B](fa: F[A])(z: B)(f: (B, A) => B): B =
    ???

  // Exercise 12.18
  def fuse[G[_], H[_], A, B](fa: F[A])(f: A => G[B], g: A => H[B])(implicit ga: Applicative[G], ha: Applicative[H]): (G[F[B]], H[F[B]]) =
    ???

  // Exercise 12.19
  def compose[G[_]](implicit g: Traverse[G]): Traverse[({ type t[x] = F[G[x]] })#t] =
    ???
}

object Traverse {

  // Exercise 12.13
  val listTraverse = ???

  // Exercise 12.13
  val optionTraverse = ???

  // Exercise 12.13
  val treeTraverse = ???
}

// Section 12.4.1
sealed trait Validation[+E, +A]

// Section 12.4.1
case class Failure[E](head: E, tail: Vector[E])
  extends Validation[E, Nothing]

// Section 12.4.1
case class Success[A](a: A) extends Validation[Nothing, A]

// Section 12.6
case class Tree[+A](head: A, tail: List[Tree[A]])