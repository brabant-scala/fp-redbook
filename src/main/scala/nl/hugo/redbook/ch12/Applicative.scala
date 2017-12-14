package nl.hugo.redbook.ch12

import language.higherKinds
import language.implicitConversions
import scala.util.Try
import nl.hugo.redbook.ch6.State
import nl.hugo.redbook.ch10.{Foldable, Monoid}
import nl.hugo.redbook.ch11.Functor
import nl.hugo.redbook.ch12.Traverse.Id

// Listing 12.1
trait Applicative[F[_]] extends Functor[F] { self =>

  // Listing 12.1
  def unit[A](a: => A): F[A]

  // Listing 12.1
  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List.empty[B]))((a, fbs) => map2(f(a), fbs)(_ :: _))

  // Exercise 12.1
  def sequence[A](fas: List[F[A]]): F[List[A]] = traverse(fas)(x => x)

  // Exercise 12.1
  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = {
    if (n <= 0) {
      unit(List.empty)
    } else {
      map2(fa, replicateM(n-1, fa))( _ :: _)
    }
  }

  def product_12_1[A,B](fa: F[A], fb: F[B]): F[(A,B)] = {
    map2(fa, fb)((a, b) => (a, b))
  }

  // Exercise 12.2
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
    apply[B, C](apply[A, B => C](unit(f.curried))(fa))(fb)
  }

  // Exercise 12.2
  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] = {
    map2[A => B, A, B](fab, fa)((f, a) => f(a))
  }

  // Exercise 12.2
  def map[A, B](fa: F[A])(f: A => B): F[B] = {
    apply(unit(f))(fa)
  }

  // Exercise 12.3
  def map3[A,B,C,D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = {
    apply(apply(apply(unit(f.curried))(fa))(fb))(fc)
  }

  def map4[A,B,C,D,E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] = {
    apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)
  }

  // Exercise 12.8
  def product[G[_]](g: Applicative[G]): Applicative[Lambda[A => (F[A], G[A])]] = new Applicative[Lambda[A => (F[A], G[A])]] {
    override def unit[A](a: => A): (F[A], G[A]) = {
      (self.unit(a), g.unit(a))
    }
    def apply[A, B](ff: (F[A => B], G[A => B]))(fa: (F[A], G[A])): (F[B], G[B]) = {
      (self.apply(ff._1)(fa._1),g.apply(ff._2)(fa._2))
    }
  }
  // Exercise 12.9
  def compose[G[_]](g: Applicative[G]): Applicative[Lambda[A => F[G[A]]]] = new Applicative[Lambda[A => F[G[A]]]] {
    override def unit[A](a: => A): F[G[A]] = self.unit(g.unit(a))
    def apply[A, B](fgf: F[G[A => B]])(fga: F[G[A]]): F[G[B]] = {
      self.map2[G[A => B], G[A], G[B]](fgf, fga)((gf, ga) => g.map2(gf, ga)((f, a) => f(a)))
    }
  }

  // Exercise 12.12
  def traverseMap[K, V, W](as: Map[K, V])(f: V => F[W]): F[Map[K, W]] =
    as.foldRight[F[Map[K, W]]](unit(Map.empty[K, W])){
      (entry, fMapKW) => {
        val key: K = entry._1
        val v: V = entry._2
        map2[W, Map[K, W], Map[K, W]](f(v), fMapKW)((w, m) => m.updated(key, w))
      }
    }

  // Exercise 12.1
  def sequenceMap[K, V](m: Map[K, F[V]]): F[Map[K, V]] = traverseMap(m)(x => x)
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
  def validationApplicative[E]: Applicative[Validation[E, ?]] = new Applicative[Validation[E, ?]] {
    override def unit[A](a: => A): Validation[E, A] = Success(a)
    override def apply[A, B](ff: Validation[E, A => B])(fa: Validation[E, A]): Validation[E, B] = {
      (ff, fa) match {
        case (Success(f), Success(a)) => Success(f(a))
        case (Success(_), x) => x
        case (x, Success(_)) => x
        case (Failure(e1, t1), Failure(e2, t2)) => (Failure(e1, t1 ++ Vector(e2) ++ t2))

      }
    }
  }

  // Listing 12.8
  type Const[A, B] = A

  // Listing 12.8
  implicit def monoidApplicative[M](m: Monoid[M]) =
    new Applicative[({ type f[x] = Const[M, x] })#f] {
      def unit[A](a: => A): M = m.zero
      override def apply[A, B](m1: M)(m2: M): M = m.op(m1, m2)
    }

  implicit val idApplicative: Applicative[Id] = new Applicative[Id] {
    def unit[A](a: => A): A = a
    override def apply[A, B](f: A=>B)(a: A): B = f(a)
  }


}

// Listing 12.1
trait Monad[F[_]] extends Applicative[F] { self =>

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
  def compose[G[_]](g: Monad[G]): Monad[Lambda[A => F[G[A]]]] = new Monad[Lambda[A => F[G[A]]]] {
    override def unit[A](a: => A): F[G[A]] = self.unit(g.unit(a))
    def flatMap[A, B](fa: F[G[A]])(ff: A => F[G[B]]): F[G[B]] = {
      self.flatMap[G[A], G[B]](fa){
        (ga: G[A]) => {
          g.flatMap(ga)(???)
        }
      }
    }
  }
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
  def eitherMonad[E]: Monad[Either[E, ?]] = new Monad[Either[E, ?]] {
    override def unit[A](a: => A): Either[E, A] = {
      Right(a)
    }
    override def flatMap[A,B](fa: Either[E, A])(ff: A => Either[E, B]): Either[E, B] = {
      fa match {
        case Right(a) => f(a)
        case Left(err) => Left(err)
      }
    }
  }

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
  def map[A, B](fa: F[A])(f: A => B): F[B] = {
    traverse[Id, A, B](fa)(f)(Applicative.idApplicative)
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
  type Id[X] = X

  // Exercise 12.13
  val listTraverse: Traverse[List] = new Traverse[List] {
    override def sequence[G[_] : Applicative, A](fga: List[G[A]]): G[List[A]] = {
      val G = implicitly[Applicative[G]]
      fga.foldRight[G[List[A]]](G.unit(List.empty[A]))(G.map2(_,_)(_ :: _))
    }
  }

  // Exercise 12.13
  val optionTraverse: Traverse[Option] = new Traverse[Option] {
    override def sequence[G[_] : Applicative, A](fga: Option[G[A]]): G[Option[A]] = {
      val G = implicitly[Applicative[G]]
      fga.fold[G[Option[A]]](G.unit(None))((ga: G[A]) => G.apply[A, Option[A]](G.unit((a:A) => Some(a)))(ga))
    }
  }

  // Exercise 12.13
  val treeApplicative: Applicative[Tree] = new Applicative[Tree] {
    override def unit[A](a: => A): Tree[A] = Tree(a, List.empty)
    override def apply[A, B](fab: Tree[A => B])(fa: Tree[A]): Tree[B] = {

    }
  }
  val treeTraverse: Traverse[Tree] = new Traverse[Tree] {
    override def sequence[G[_] : Applicative, A](fga: Tree[G[A]]): G[Tree[A]] = {
      val G = implicitly[Applicative[G]]
      val head: G[A] = fga.head
      val tail: List[Tree[G[A]]] = fga.tail

      val f:List[G[Tree[A]]] = tail.map(tga => sequence(tga))
      val x: Int =
      G.map2[A, List[Tree[A]], Tree[A]](head, ???)(Tree(_,_))
    }
  }
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