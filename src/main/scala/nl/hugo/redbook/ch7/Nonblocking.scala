package nl.hugo.redbook.ch7

import java.util.concurrent.{ Callable, CountDownLatch, ExecutorService }
import java.util.concurrent.atomic.AtomicReference

object Nonblocking {

  trait Future[+A] {
    private[ch7] def apply(k: A => Unit): Unit
  }

  type Par[+A] = ExecutorService => Future[A]

  object Par {

    // listing 7.6
    def run[A](es: ExecutorService)(p: Par[A]): A = {
      val ref = new AtomicReference[A]
      val latch = new CountDownLatch(1)
      p(es) { a =>
        ref.set(a)
        latch.countDown()
      }
      latch.await()
      ref.get
    }

    // section 7.4.4
    def unit[A](a: A): Par[A] =
      es => new Future[A] {
        def apply(cb: A => Unit): Unit =
          cb(a)
      }

    // section 7.4.4
    def fork[A](a: => Par[A]): Par[A] =
      es => new Future[A] {
        def apply(cb: A => Unit): Unit =
          eval(es)(a(es)(cb))
      }

    // section 7.4.4
    private def eval(es: ExecutorService)(r: => Unit): Unit =
      es.submit(new Callable[Unit] { def call = r })

    // listing 7.4
    def lazyUnit[A](a: => A): Par[A] =
      fork(unit(a))

    // exercise 7.4
    def asyncF[A, B](f: A => B): A => Par[B] =
      a => lazyUnit(f(a))

    // listing 7.7
    def map2[A, B, C](p: Par[A], p2: Par[B])(f: (A, B) => C): Par[C] =
      es => new Future[C] {
        def apply(cb: C => Unit): Unit = {
          var ar: Option[A] = None
          var br: Option[B] = None
          val combiner = Actor[Either[A, B]](es) {
            case Left(a) =>
              if (br.isDefined) eval(es)(cb(f(a, br.get)))
              else ar = Some(a)
            case Right(b) =>
              if (ar.isDefined) eval(es)(cb(f(ar.get, b)))
              else br = Some(b)
          }
          p(es)(a => combiner ! Left(a))
          p2(es)(b => combiner ! Right(b))
        }
      }

    // specialized version of `map`
    def map[A, B](p: Par[A])(f: A => B): Par[B] =
      es => new Future[B] {
        def apply(cb: B => Unit): Unit =
          p(es)(a => eval(es) { cb(f(a)) })
      }

    // Exercises

    def choice[A](p: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
      es => new Future[A] {
        def apply(cb: A => Unit): Unit =
          p(es) { b =>
            if (b) eval(es) { t(es)(cb) }
            else eval(es) { f(es)(cb) }
          }
      }

    // exercise 7.11
    def choiceN[A](pn: Par[Int])(choices: List[Par[A]]): Par[A] =
      es => new Future[A] {
        def apply(cb: A => Unit): Unit = {
          pn(es) { n =>
            val par = choices(n)
            eval(es) { par(es)(cb) }
          }
        }
      }

    // exercise 7.11
    def choiceViaChoiceN[A](cond: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] =
      choiceN(map(cond)(b => if (b) 0 else 1))(List(ifTrue, ifFalse))

    // exercise 7.12
    def choiceMap[K, V](pk: Par[K])(choices: Map[K, Par[V]]): Par[V] =
      es => new Future[V] {
        def apply(cb: V => Unit): Unit = {
          pk(es) { k =>
            val par = choices(k)
            eval(es) { par(es)(cb) }
          }
        }
      }

    // exercise 7.13
    def chooser[A, B](pa: Par[A])(f: A => Par[B]): Par[B] =
      es => new Future[B] {
        def apply(cb: B => Unit): Unit = {
          pa(es) { a =>
            val par = f(a)
            eval(es) { par(es)(cb) }
          }
        }
      }

    // exercise 7.13
    def flatMap[A, B](pa: Par[A])(f: A => Par[B]): Par[B] =
      chooser(pa)(f)

    // exercise 7.13
    def choiceViaChooser[A](cond: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] =
      chooser(cond)(b => if (b) ifTrue else ifFalse)

    // exercise 7.13
    def choiceNViaChooser[A](pn: Par[Int])(choices: List[Par[A]]): Par[A] =
      chooser(pn)(i => choices(i))

    // exercise 7.14
    def join[A](ppa: Par[Par[A]]): Par[A] =
      es => new Future[A] {
        def apply(cb: A => Unit): Unit = {
          ppa(es) { pa =>
            eval(es) { pa(es)(cb) }
          }
        }
      }

    // exercise 7.14
    def joinViaFlatMap[A](ppa: Par[Par[A]]): Par[A] =
      flatMap(ppa)(identity)

    // exercise 7.14
    def flatMapViaJoin[A, B](pa: Par[A])(f: A => Par[B]): Par[B] =
      join(map(pa)(f))

    /** Provides infix syntax for Par. */
    implicit class ParOps[A](val p: Par[A]) extends AnyVal {
      def map[B](f: A => B): Par[B] = Par.map(p)(f)
      def flatMap[B](f: A => Par[B]): Par[B] = Par.flatMap(p)(f)
      def map2[B, C](b: Par[B])(f: (A, B) => C): Par[C] = Par.map2(p, b)(f)
      def zip[B](b: Par[B]): Par[(A, B)] = map2(b)((_, _))
    }
  }
}