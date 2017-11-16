package nl.hugo.redbook.ch11

import nl.hugo.redbook.ch6.State
import nl.hugo.redbook.ch7.Par
import nl.hugo.redbook.ch7.Par._
import nl.hugo.redbook.ch8.Gen
import nl.hugo.redbook.ch8.Gen.{ listOfN, unit }
import nl.hugo.redbook.ch9.LocationParser.Parser
import nl.hugo.redbook.ch9.Parsers

import language.higherKinds

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
    lma.foldRight(unit(List.empty[A]))((ma: M[A], mla: M[List[A]]) => map2(ma, mla)(_ :: _))

  // Exercise 11.3
  def traverse[A, B](la: List[A])(f: A => M[B]): M[List[B]] =
    la.foldRight(unit(List.empty[B]))((a: A, mlb: M[List[B]]) => map2(f(a), mlb)(_ :: _))

  // Exercise 11.4
  def replicateM[A](n: Int, ma: M[A]): M[List[A]] = {
    if (n <= 0) {
      unit(List.empty[A])
    } else {
      map2(ma, replicateM(n - 1, ma))(_ :: _)
    }
  }

  // Exercise 11.6
  def filterM[A](ms: List[A])(f: A => M[Boolean]): M[List[A]] = {
    ms.foldRight(unit(List.empty[A])) {
      (a: A, mla: M[List[A]]) =>
        {
          map2(f(a), mla) {
            (b, lst) => if (b) a :: lst else lst
          }
        }
    }
  }

  // Exercise 11.7
  def compose[A, B, C](f: A => M[B], g: B => M[C]): A => M[C] = {
    (a: A) => flatMap(f(a))(g)
  }

  // Exercise 11.8
  // Implement in terms of `compose`:
  def identity[A](x: A): A = x
  def _flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] = {
    compose[M[A], A, B](identity[M[A]], f)(ma)
  }

  // Exercise 11.12
  def join[A](mma: M[M[A]]): M[A] = {
    flatMap(mma)(identity)
  }

  // Exercise 11.13
  // Implement in terms of `join`:
  def __flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] = {
    join(map(ma)(f))
  }
  def __compose[A, B, C](f: A => M[B], g: B => M[C]): A => M[C] = {
    (a: A) => join(map(f(a))(g))
  }

  // Exercise 11.14
  // associativity of kleisli composition
  // compose(f, compose(g, h)) == compose(compose(f, g), h)
  // identity laws
  // compose(unit, f) = compose(f, unit) == f
  def identityLaw_LeftUnit[A, B](f: A => M[B]) =
    (a: A) => join(map(unit(a))(f))
}

case class Reader[R, A](run: R => A)

object Monad {
  val genMonad = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)
    override def flatMap[A, B](ma: Gen[A])(f: A => Gen[B]): Gen[B] = ma flatMap f
  }

  // Exercise 11.1
  val parMonad: Monad[Par] = new Monad[Par] {
    override def unit[A](a: => A): Par[A] = Par.unit(a)
    override def flatMap[A, B](ma: Par[A])(f: A => Par[B]): Par[B] = Par.flatMap(ma)(f)

  }

  // Exercise 11.1
  def parserMonad[P[+_]](p: Parsers[P]): Monad[P] = new Monad[P] {
    import p.ParserOps
    override def unit[A](a: => A) = p.succeed(a)
    override def flatMap[A, B](ma: P[A])(f: A => P[B]) = p.flatMap(ma)(f)
  }

  // Exercise 11.1
  val optionMonad: Monad[Option] = new Monad[Option] {
    override def unit[A](a: => A): Option[A] = Option(a)
    override def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] = ma.flatMap(f)
  }

  // Exercise 11.1
  val streamMonad: Monad[Stream] = new Monad[Stream] {
    override def unit[A](a: => A) = Stream(a)
    override def flatMap[A, B](ma: Stream[A])(f: A => Stream[B]) = ma.flatMap(f)
  }

  // Exercise 11.1
  val listMonad: Monad[List] = new Monad[List] {
    override def unit[A](a: => A) = List(a)
    override def flatMap[A, B](ma: List[A])(f: A => List[B]) = ma.flatMap(f)
  }

  // Exercise 11.2
  def stateMonad[S] = new Monad[Lambda[A => State[S, A]]] {
    override def unit[A](a: => A): State[S, A] = State.unit[S, A](a)
    override def flatMap[A, B](ma: State[S, A])(f: A => State[S, B]): State[S, B] = ma.flatMap(f)
  }

  // Exercise 11.17
  val idMonad: Monad[Id] = new Monad[Id] {
    override def unit[A](a: => A) = Id(a)

    override def flatMap[A, B](ma: Id[A])(f: A => Id[B]) = f(ma.value)
  }

  // Exercise 11.20
  def readerMonad[R] = new Monad[Lambda[A => Reader[R, A]]] {
    def unit[A](a: => A): Reader[R, A] = Reader(_ => a)
    override def flatMap[A, B](st: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] = {
      Reader((r: R) => {
        f(st.run(r)).run(r)
      })
    }
  }
}

case class Id[A](value: A) {
  // Exercise 11.17
  def map[B](f: A => B): Id[B] = Id(f(value))
  // Exercise 11.17
  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}
