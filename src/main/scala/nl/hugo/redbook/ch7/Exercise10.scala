package nl.hugo.redbook.ch7

import java.util.concurrent.{ Callable, CountDownLatch, ExecutorService }
import java.util.concurrent.atomic.AtomicReference

// the (standard) FP answer to this exercise is to change an exception (= side effect) into a value
// typically this is called a Try value, with a Success and Failure implementations

object Exercise10 {

  sealed trait Try[A]
  case class Success[A](a: A) extends Try[A]
  case class Failure[A](e: Exception) extends Try[A]
  object Try {
    def apply[A](a: => A): Try[A] =
      try {
        Success(a)
      } catch {
        case e: Exception => Failure(e)
      }
  }

  // from section 7.4.4
  sealed trait Future[A] {
    private[Exercise10] def apply(k: Try[A] => Unit): Unit
  }

  // from section 7.4.4
  type Par[A] = ExecutorService => Future[A]

  object Par {

    // from listing 7.6
    def run[A](es: ExecutorService)(p: Par[A]): A = {
      val ref = new AtomicReference[Try[A]]
      val latch = new CountDownLatch(1)
      p(es) { a =>
        ref.set(a)
        latch.countDown()
      }
      latch.await()
      ref.get match {
        case Success(a) => a
        case Failure(e) => throw e
      }
    }

    // from section 7.4.4
    def unit[A](a: A): Par[A] =
      es => new Future[A] {
        def apply(cb: Try[A] => Unit): Unit =
          cb(Success(a))
      }

    // from listing 7.4 (common)
    def lazyUnit[A](a: => A): Par[A] =
      fork(unit(a))

    // from section 7.4.4
    def fork[A](p: => Par[A]): Par[A] =
      es => new Future[A] {
        def apply(cb: Try[A] => Unit): Unit = {
          eval(es) {
            Try(p(es)) match {
              case Success(f) => f(cb)
              case Failure(e) => cb(Failure(e))
            }
          }
        }
      }

    // from section 7.4.4
    def eval(es: ExecutorService)(r: => Unit): Unit =
      es.submit(new Callable[Unit] { def call = r })

    // from listing 7.7
    def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] =
      es => new Future[C] {
        def apply(cb: Try[C] => Unit): Unit = {
          var ra: Option[A] = None
          var rb: Option[B] = None
          val combiner = Actor[Either[Try[A], Try[B]]](es) {
            case Left(Success(a)) => rb match {
              case None => ra = Some(a)
              case Some(b) => eval(es)(cb(Try(f(a, b))))
            }
            case Right(Success(b)) => ra match {
              case None => rb = Some(b)
              case Some(a) => eval(es)(cb(Try(f(a, b))))
            }
            case Left(Failure(e)) => eval(es)(cb(Failure[C](e)))
            case Right(Failure(e)) => eval(es)(cb(Failure[C](e)))
          }
          pa(es)(a => combiner ! Left(a))
          pb(es)(b => combiner ! Right(b))
        }
      }
  }
}