package nl.hugo.redbook.ch11

import language.higherKinds
import nl.hugo.redbook.ch6.State
import nl.hugo.redbook.ch7.Nonblocking.Par
import nl.hugo.redbook.ch8.Gen
import nl.hugo.redbook.ch9.Parsers

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]

  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] =
    e match {
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
    traverse(lma)(identity)

  // Exercise 11.3
  def traverse[A, B](la: List[A])(f: A => M[B]): M[List[B]] =
    la.foldRight[M[List[B]]](unit(Nil))((a, mlb) => map2(f(a), mlb)(_ :: _))

  // Exercise 11.4
  def replicateM[A](n: Int, ma: M[A]): M[List[A]] =
    if (n > 0) map2(ma, replicateM(n - 1, ma))(_ :: _)
    else unit(Nil)

  // Exercise 11.6
  def filterM[A](ms: List[A])(f: A => M[Boolean]): M[List[A]] =
    ms.foldRight[M[List[A]]](unit(Nil)) { (a, mla) =>
      flatMap(f(a)) {
        case true => map2(unit(a), mla)(_ :: _)
        case false => mla
      }
    }

  // Exercise 11.7
  def compose[A, B, C](f: A => M[B], g: B => M[C]): A => M[C] =
    a => flatMap(f(a))(g)

  // Exercise 11.8
  // Implement in terms of `compose`:
  def _flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] =
    compose[Unit, A, B](_ => ma, f)(())

  // Exercise 11.9
  // compose(compose(f, g), h) == compose(f, compose(g, h))
  // a => flatMap(compose(f, g)(a))(h) == a => flatMap(f(a))(compose(g, h))
  // a => flatMap(_.flatMap(f)(g))(h) == a => flatMap(f(a))(_.flatMap(g)(h))
  // x.flatMap(f).flatMap(g)
  // TODO: complete
  // x.flatMap(f).flatMap(g) == x.flatMap(a => f(a).flatMap(g))

  // Exercise 11.10
  // left identity:
  //   compose(f, unit) ==
  //   a => flatMap(f(a))(unit) ==
  //   a => f(a) ==
  //   f
  // right identity:
  //   compose(unit, f) ==
  //   a => flatMap(unit(a))(f) ==
  //   a => f(a) ==
  //   f

  // Exercise 11.11
  // compose(Some(_))(unit) ==
  // a => flatMap(Some(a))(unit) ==
  //

  // Exercise 11.12
  def join[A](mma: M[M[A]]): M[A] =
    flatMap(mma)(identity)

  // Exercise 11.13
  // Implement in terms of `join`:
  def __flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] =
    join(map(ma)(f))

  // Exercise 11.13
  def __compose[A, B, C](f: A => M[B], g: B => M[C]): A => M[C] =
    a => join(map(f(a))(g))

  // Exercise 11.14
  // associativity:  join(map(join(map(f)(g)))(h)) == join(map(f, join(map(f)(g))))
  // left identity:  join(map(f)(unit)) == f
  // right identity: join(map(unit)(f)) == f
}

case class Reader[R, A](run: R => A)

object Monad {
  val genMonad = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)
    override def flatMap[A, B](ma: Gen[A])(f: A => Gen[B]): Gen[B] = ma flatMap f
  }

  // Exercise 11.1
  val parMonad = new Monad[Par] {
    def unit[A](a: => A): Par[A] = Par.unit(a)
    def flatMap[A, B](ma: Par[A])(f: A => Par[B]): Par[B] = Par.flatMap(ma)(f)
  }

  // Exercise 11.1
  def parserMonad[P[+_]](p: Parsers[P]) = new Monad[P] {
    def unit[A](a: => A): P[A] = p.succeed(a)
    def flatMap[A, B](ma: P[A])(f: A => P[B]): P[B] = p.flatMap(ma)(f)
  }

  // Exercise 11.1
  val optionMonad = new Monad[Option] {
    def unit[A](a: => A): Option[A] = Some(a)
    def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] = ma.flatMap(f)
  }

  // Exercise 11.1
  val streamMonad = new Monad[Stream] {
    def unit[A](a: => A): Stream[A] = Stream(a)
    def flatMap[A, B](ma: Stream[A])(f: A => Stream[B]): Stream[B] = ma.flatMap(f)
  }

  // Exercise 11.1
  val listMonad = new Monad[List] {
    def unit[A](a: => A): List[A] = List(a)
    def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] = ma.flatMap(f)
  }

  // Exercise 11.2
  def stateMonad[S] = new StateMonad[S].monad

  class StateMonad[S] {
    type SState[A] = State[S, A]

    val monad = new Monad[SState] {
      def unit[A](a: => A): State[S, A] = State.unit[S, A](a)
      def flatMap[A, B](ma: State[S, A])(f: A => State[S, B]): State[S, B] = ma.flatMap(f)
    }
  }

  // Exercise 11.17
  val idMonad = new Monad[Id] {
    def unit[A](a: => A): Id[A] = Id(a)
    def flatMap[A, B](ma: Id[A])(f: A => Id[B]): Id[B] = ma.flatMap(f)
  }

  // Exercise 11.20
  def readerMonad[R] = new Monad[({ type f[x] = Reader[R, x] })#f] {
    def unit[A](a: => A): Reader[R, A] =
      Reader(_ => a)
    override def flatMap[A, B](st: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] =
      Reader(r => f(st.run(r)).run(r))
  }
}

case class Id[A](value: A) {

  // Exercise 11.17
  def map[B](f: A => B): Id[B] =
    Id(f(value))

  // Exercise 11.17
  def flatMap[B](f: A => Id[B]): Id[B] =
    f(value)
}
