package nl.hugo.redbook.ch12

import language.{ higherKinds, implicitConversions }
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
    fas.foldRight(unit(List.empty[A]))((fa, fs) => map2(fa, fs)(_ :: _))

  // Exercise 12.1
  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  // Exercise 12.1
  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)((a, b) => (a, b))

  // Exercise 12.2
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    map2_2(fa, fb)(f)

  // Exercise 12.2
  def map2_1[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply(map(fa)(a => (b: B) => f(a, b)))(fb)

  // Exercise 12.2
  def map2_2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply(map(fa)(f.curried))(fb)

  // Exercise 12.2
  def map2_3[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply(unit(f.tupled))(product(fa, fb)) // TODO: is this looping infinitely?

  // Exercise 12.2
  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fa, fab)((a, f) => f(a))

  // Exercise 12.2
  def map[A, B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)

  // Exercise 12.3
  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
    apply(apply(apply(unit(f.curried))(fa))(fb))(fc)

  // Exercise 12.3
  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] =
    apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)

  // Exercise 12.8
  def product[G[_]](g: Applicative[G]): Applicative[({ type t[x] = (F[x], G[x]) })#t] = {
    val f = this
    new Applicative[({ type t[x] = (F[x], G[x]) })#t] {
      def unit[A](a: => A) = (f.unit(a), g.unit(a))
      override def apply[A, B](fab: (F[A => B], G[A => B]))(fga: (F[A], G[A])): (F[B], G[B]) = {
        val fb = f.apply(fab._1)(fga._1)
        val gb = g.apply(fab._2)(fga._2)
        (fb, gb)
      }
    }
  }

  // Exercise 12.9
  def compose[G[_]](g: Applicative[G]): Applicative[({ type t[x] = F[G[x]] })#t] = {
    val f = this
    new Applicative[({ type t[x] = (F[G[x]]) })#t] {
      def unit[A](a: => A) = f.unit(g.unit(a))
      override def map2[A, B, C](fga: F[G[A]], fgb: F[G[B]])(fab: (A, B) => C): F[G[C]] = {
        val gab: (G[A], G[B]) => G[C] =
          (ga, gb) => g.map2(ga, gb)(fab)
        f.map2(fga, fgb)(gab)
      }
    }
  }

  // Exercise 12.10
  // TODO

  // Exercise 12.12
  def sequenceMap[K, V](m: Map[K, F[V]]): F[Map[K, V]] =
    m.foldLeft(unit(Map.empty[K, V])) {
      case (fmkv, (k, fv)) => map2(fmkv, fv) {
        (m, v) => m + (k -> v)
      }
    }
}

object Applicative {

  // Section 12.4.1
  val streamApplicative = new Applicative[Stream] {
    def unit[A](a: => A): Stream[A] =
      Stream.continually(a)
    override def map2[A, B, C](a: Stream[A], b: Stream[B])(f: (A, B) => C): Stream[C] =
      a.zip(b).map(f.tupled)
  }

  // Exercise 12.4
  // def sequence[A](a: List[Stream[A]]): Stream[List[A]]
  // Turns a list of streams into a stream of lists.
  // Basically, this can be seen as turning rows of values into a row of columns.

  // Exercise 12.6
  def validationApplicative[E]: Applicative[({ type t[x] = Validation[E, x] })#t] =
    new Applicative[({ type t[x] = Validation[E, x] })#t] {
      def unit[A](a: => A): Validation[E, A] = Success(a)
      override def map2[A, B, C](va: Validation[E, A], vb: Validation[E, B])(f: (A, B) => C): Validation[E, C] =
        (va, vb) match {
          case (Success(a), Success(b)) => Success(f(a, b))
          case (f @ Failure(_, _), Success(_)) => f
          case (Success(_), f @ Failure(_, _)) => f
          case (Failure(ah, at), Failure(bh, bt)) => Failure(ah, (at :+ bh) ++ bt)
        }
    }

  // Exercise 12.7
  // TODO

  // Listing 12.8
  type Const[A, B] = A

  // Listing 12.8
  implicit def monoidApplicative[M](m: Monoid[M]): Applicative[({ type t[x] = Const[M, x] })#t] =
    new Applicative[({ type t[x] = Const[M, x] })#t] {
      def unit[A](a: => A): M = m.zero
      override def map2[A, B, C](m1: M, m2: M)(f: (A, B) => C): M = m.op(m1, m2)
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
  def stateMonad[S]: Monad[({ type t[x] = State[S, x] })#t] =
    new Monad[({ type t[x] = State[S, x] })#t] {
      def unit[A](a: => A): State[S, A] = State.unit[S, A](a)
      override def flatMap[A, B](ma: State[S, A])(f: A => State[S, B]): State[S, B] = ma.flatMap(f)
    }

  // Exercise 12.5
  def eitherMonad[E]: Monad[({ type t[x] = Either[E, x] })#t] =
    new Monad[({ type t[x] = Either[E, x] })#t] {
      def unit[A](a: => A): Either[E, A] = Right(a)
      override def flatMap[A, B](ea: Either[E, A])(f: A => Either[E, B]): Either[E, B] =
        ea match {
          case Right(a) => f(a)
          case Left(e) => Left(e)
        }
    }

  // Exercise 12.20
  def composeM[F[_], G[_]](implicit f: Monad[F], g: Monad[G], t: Traverse[G]): Monad[({ type t[x] = F[G[x]] })#t] =
    new Monad[({ type t[x] = F[G[x]] })#t] {
      def unit[A](a: => A): F[G[A]] = f.unit(g.unit(a))
      override def flatMap[A, B](fga: F[G[A]])(afgb: A => F[G[B]]): F[G[B]] =
        f.flatMap(fga) { ga =>
          val fggb: F[G[G[B]]] = t.traverse(ga)(afgb)
          f.map(fggb)(g.join)
        }
    }
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
  def map[A, B](fa: F[A])(f: A => B): F[B] = {
    implicit val app = Identity.applicative
    traverse[Identity, A, B](fa)(a => Identity(f(a))).value
  }

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
    mapAccum(fa, toList(fa).reverse)((_, as) => (as.head, as.tail))._1

  // Exercise 12.17
  def foldLeft[A, B](fa: F[A])(z: B)(f: (B, A) => B): B =
    mapAccum(fa, z)((a, b) => ((), f(b, a)))._2

  // TODO: more efficient?
  def foldRight[A, B](fa: F[A])(z: B)(f: (A, B) => B): B =
    foldLeft(reverse(fa))(z)((b, a) => f(a, b))

  // Listing 12.12
  def zip[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    mapAccum(fa, toList(fb)) {
      case (a, Nil) => sys.error("zip: Incompatible shapes.")
      case (a, b :: bs) => ((a, b), bs)
    }._1

  // Listing 12.13
  def zipL[A, B](fa: F[A], fb: F[B]): F[(A, Option[B])] =
    mapAccum(fa, toList(fb)) {
      case (a, Nil) => ((a, None), Nil)
      case (a, b :: bs) => ((a, Some(b)), bs)
    }._1

  // Listing 12.13
  def zipR[A, B](fa: F[A], fb: F[B]): F[(Option[A], B)] =
    mapAccum(fb, toList(fa)) {
      case (b, Nil) => ((None, b), Nil)
      case (b, a :: as) => ((Some(a), b), as)
    }._1

  // Exercise 12.18
  def fuse[G[_], H[_], A, B](fa: F[A])(f: A => G[B], g: A => H[B])(implicit ga: Applicative[G], ha: Applicative[H]): (G[F[B]], H[F[B]]) =
    traverse[({ type t[x] = (G[x], H[x]) })#t, A, B](fa)(a => (f(a), g(a)))(ga product ha)

  // Exercise 12.19
  def compose[G[_]](implicit g: Traverse[G]): Traverse[({ type t[x] = F[G[x]] })#t] = {
    val f = this
    new Traverse[({ type t[x] = F[G[x]] })#t] {
      override def traverse[H[_]: Applicative, A, B](fga: F[G[A]])(ahb: A => H[B]) =
        f.traverse(fga)(ga => g.traverse(ga)(ahb))
    }
  }
}

object Traverse {

  // Exercise 12.13
  val listTraverse = new Traverse[List] {
    override def traverse[G[_], A, B](as: List[A])(f: A => G[B])(implicit g: Applicative[G]): G[List[B]] =
      as.foldRight(g.unit(List.empty[B]))((a, fbs) => g.map2(f(a), fbs)(_ :: _))
  }

  // Exercise 12.13
  val optionTraverse = new Traverse[Option] {
    override def traverse[G[_], A, B](oa: Option[A])(f: A => G[B])(implicit g: Applicative[G]): G[Option[B]] =
      oa match {
        case Some(a) => g.map(f(a))(Some.apply)
        case None => g.unit(None)
      }
  }

  // Exercise 12.13
  val treeTraverse = new Traverse[Tree] {
    override def traverse[G[_], A, B](at: Tree[A])(f: A => G[B])(implicit g: Applicative[G]): G[Tree[B]] = {
      val gh = f(at.head)
      val gt = listTraverse.traverse(at.tail)(t => traverse(t)(f))
      g.map2(gh, gt)(Tree.apply)
    }
  }
}

// Exercise 12.14
case class Identity[A](value: A)

object Identity {

  // Exercise 12.14
  val applicative = new Applicative[Identity] {
    def unit[A](a: => A): Identity[A] = Identity(a)
    override def map2[A, B, C](a: Identity[A], b: Identity[B])(f: (A, B) => C): Identity[C] = Identity(f(a.value, b.value))
  }
}

// Section 12.4.1
sealed trait Validation[+E, +A]

// Section 12.4.1
case class Failure[E](head: E, tail: Vector[E] = Vector.empty) extends Validation[E, Nothing]

// Section 12.4.1
case class Success[A](a: A) extends Validation[Nothing, A]

// Section 12.6
case class Tree[+A](head: A, tail: List[Tree[A]])