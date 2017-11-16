package nl.hugo.redbook.ch11

import nl.hugo.redbook.ch6.State
import nl.hugo.redbook.ch7.Par
import nl.hugo.redbook.ch7.Par._
import nl.hugo.redbook.ch8.Gen

import language.higherKinds


trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]

  def distribute[A,B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A,B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa) => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }
}

object Functor {
  val listFunctor = new Functor[List] {
    def map[A,B](as: List[A])(f: A => B): List[B] = as map f
  }
}

trait Monad[M[_]] extends Functor[M] {
  def unit[A](a: => A): M[A]
  def flatMap[A,B](ma: M[A])(f: A => M[B]): M[B]

  def map[A,B](ma: M[A])(f: A => B): M[B] =
    flatMap(ma)(a => unit(f(a)))
  def map2[A,B,C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  // Exercise 11.3
  def sequence[A](lma: List[M[A]]): M[List[A]] =
    traverse(lma)(x => x)

  // Exercise 11.3
  def traverse[A,B](la: List[A])(f: A => M[B]): M[List[B]] =
    la.foldRight(unit(List.empty): M[List[B]])((oa,ola) => map2(f(oa), ola)(_ :: _))

  // Exercise 11.4
  def replicateM[A](n: Int, ma: M[A]): M[List[A]] =
    sequence(List.fill(n)(ma))

  // Exercise 11.6
  def filterM[A](ms: List[A])(f: A => M[Boolean]): M[List[A]] =
    // Solution 1.
    map2(unit(ms), sequence(ms.map(f)))((mms, mfm) => (mfm zip mms).filter(_._1).map(_._2))
    // Solution 2.
//  {
//    val list = ms.map(a => (f(a), a) match {
//      case (mb: M[Boolean], _) => map(mb)(b => if (b) List(a) else List.empty)
//    })
//    flatMap(sequence(list))(x => x)
//  }

  // Exercise 11.7
  def compose[A,B,C](f: A => M[B], g: B => M[C]): A => M[C] =
    a => flatMap(f(a))(g)

  // Exercise 11.8
  // Implement in terms of `compose`:
  def _flatMap[A,B](ma: M[A])(f: A => M[B]): M[B] =
    compose((_:Unit) => ma, f)(())

  // Exercise 11.12
  def join[A](mma: M[M[A]]): M[A] =
    flatMap(mma)(x => x)

  // Exercise 11.13
  // Implement in terms of `join` and `map`:
  def __flatMap[A,B](ma: M[A])(f: A => M[B]): M[B] =
    join(map(ma)(f))
}

case class Reader[R, A](run: R => A)

object Monad {
  val genMonad = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)
    override def flatMap[A,B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
      ma flatMap f
  }

  // Exercise 11.1
  val parMonad: Monad[Par] = new Monad[Par] {
    override def unit[A](a: => A) = Par.unit(a)
    override def flatMap[A, B](ma: Par[A])(f: A => Par[B]) = Par.flatMap(ma)(f)
  }

  // Exercise 11.1
//  def parserMonad[P[+_]](p: Parsers[P]): Monad[P] = new Monad[P] {
//    override def unit[A](a: => A): P[A] = Parsers
//    override def flatMap[A, B](ma: P[A])(f: A => P[B]): P[B] = ???
//  }

  // Exercise 11.1
  val optionMonad: Monad[Option] = new Monad[Option] {
    override def unit[A](a: => A) = Option(a)
    override def flatMap[A, B](ma: Option[A])(f: A => Option[B]) = ma flatMap f
  }

  // Exercise 11.1
  val streamMonad: Monad[Stream] = new Monad[Stream] {
    override def unit[A](a: => A): Stream[A] = Stream(a)
    override def flatMap[A, B](ma: Stream[A])(f: A => Stream[B]): Stream[B] = ma flatMap f
  }

  // Exercise 11.1
  val listMonad: Monad[List] = new Monad[List] {
    override def unit[A](a: => A) = List(a)
    override def flatMap[A, B](ma: List[A])(f: A => List[B]) = ma flatMap f
  }

  // Exercise 11.2
  def stateMonad[S] = new Monad[({type f[x] = State[S,x]})#f] {
    override def unit[A](a: => A): State[S, A] = State.unit(a)
    override def flatMap[A, B](ma: State[S, A])(f: A => State[S,B]): State[S, B] = flatMap(ma)(f)
  }

  // Exercise 11.17
  val idMonad: Monad[Id] = new Monad[Id] {
    override def unit[A](a: => A): Id[A] = Id(a)
    override def flatMap[A, B](ma: Id[A])(f: A => Id[B]): Id[B] = ma flatMap f
  }

  // Exercise 11.20
  def readerMonad[R] = new Monad[({type f[x] = Reader[R,x]})#f] {
    def unit[A](a: => A): Reader[R,A] = Reader(_ => a)
    override def flatMap[A,B](st: Reader[R,A])(f: A => Reader[R,B]): Reader[R,B] = flatMap(st)(f)
  }
}

case class Id[A](value: A) {
  // Exercise 11.17
  def map[B](f: A => B): Id[B] = Id(f(value))
  // Exercise 11.17
  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}
