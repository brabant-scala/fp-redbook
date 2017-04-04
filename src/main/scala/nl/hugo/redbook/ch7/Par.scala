package nl.hugo.redbook.ch7

import java.util.concurrent._
import java.util.concurrent.atomic.AtomicBoolean

object Par {

  type Par[A] = ExecutorService => Future[A]

  // listing 7.6 - blocking implementation
  def run[A](s: ExecutorService)(a: Par[A]): A =
    a(s).get

  // listing 7.5
  def unit[A](a: A): Par[A] =
    _ => UnitFuture(a)

  // listing 7.5
  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  // listing 7.5
  def fork[A](p: => Par[A]): Par[A] =
    es => es.submit(
      new Callable[A] {
        def call = p(es).get
      }
    )

  // from listing 7.4
  def lazyUnit[A](a: => A): Par[A] =
    fork(unit(a))

  // listing 7.5
  def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] =
    es => {
      val af = pa(es)
      val bf = pb(es)
      UnitFuture(f(af.get, bf.get)) // does not respect timeouts
    }

  // exercise 7.3
  def map2WhileRespectingContracts[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] =
    es => new Future[C] {
      private var value: Option[C] = None
      private var cancelled = new AtomicBoolean(false)
      private var running = new AtomicBoolean(false)
      def isDone = value.isDefined
      def get(timeout: Long, units: TimeUnit): C = {
        checkCancelled()
        running.set(true)
        val start = System.currentTimeMillis
        val a = pa(es).get(timeout, units)
        val elapsed = System.currentTimeMillis - start
        val left = units.convert(timeout, TimeUnit.MILLISECONDS) - elapsed
        checkCancelled()
        val b = pb(es).get(left, TimeUnit.MILLISECONDS)
        checkCancelled()
        val c = f(a, b)
        checkCancelled()
        value = Some(c)
        c
      }
      def get = get(7, TimeUnit.DAYS)
      def isCancelled = cancelled.get
      def cancel(evenIfRunning: Boolean) =
        if (running.get && !evenIfRunning) false
        else {
          cancelled.set(true)
          true
        }
      private def checkCancelled(): Unit =
        if (isCancelled) throw new CancellationException
    }

  // section 7.3
  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  // section 7.3
  def parMap[A, B](as: List[A])(f: A => B): Par[List[B]] =
    fork {
      val bs: List[Par[B]] = as.map(asyncF(f))
      sequence_1(bs) // TODO: calling sequence() uses (too) many threads
    }

  // section 7.3
  def sortPar(parList: Par[List[Int]]): Par[List[Int]] =
    map(parList)(_.sorted)

  // section 7.4.1
  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

  // from section 7.4.3
  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)

  // from section 7.5
  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es =>
      if (run(es)(cond)) t(es) // blocking on cond!
      else f(es)

  /** Provides infix syntax for Par. */
  implicit class ParOps[A](val p: Par[A]) extends AnyVal {
    def map[B](f: A => B): Par[B] = Par.map(p)(f)
    def flatMap[B](f: A => Par[B]): Par[B] = Par.flatMap(p)(f)
  }

  // Exercises

  // exercise 7.4
  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  // exercise 7.5
  def sequence[A](l: List[Par[A]]): Par[List[A]] =
    sequence_1(l)

  // exercise 7.5 - linear, sequential
  def sequence_1[A](l: List[Par[A]]): Par[List[A]] =
    l.foldRight(unit[List[A]](Nil)) { (p, ps) =>
      map2(p, ps)(_ :: _)
    }

  // exercise 7.5 - binary, parallel
  def sequence_2[A](l: List[Par[A]]): Par[List[A]] = {

    def go(ps: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] =
      ps match {
        case IndexedSeq() => unit(Vector.empty[A])
        case IndexedSeq(a) => map(a)(Vector(_))
        case _ => fork {
          val (l, r) = ps.splitAt(ps.size / 2)
          map2(go(l), go(r))(_ ++ _)
        }
      }

    map(go(l.toVector))(_.toList)
  }

  // experiment for section 7.3 (1st bullet)
  def parReduce[A](as: IndexedSeq[A])(f: (A, A) => A): Par[A] =
    if (as.length <= 1)
      unit(as.headOption.getOrElse(throw new Exception("empty.reduce")))
    else {
      val (l, r) = as.splitAt(as.length / 2)
      map2(fork(parReduce(l)(f)), fork(parReduce(r)(f)))(f)
    }

  // experiment for section 7.3 (1st bullet)
  def parMax(is: IndexedSeq[Int]): Par[Int] =
    parReduce(is)(_ max _)

  // experiment for section 7.3 (3rd bullet)
  def map3[A, B, C, D](pa: Par[A], pb: Par[B], pc: Par[C])(f: (A, B, C) => D): Par[D] = {
    val ab = map2(pa, pb)((_, _))
    map2(ab, pc) { case ((a, b), c) => f(a, b, c) }
  }

  // experiment for section 7.3 (3rd bullet)
  def map4[A, B, C, D, E](pa: Par[A], pb: Par[B], pc: Par[C], pd: Par[D])(f: (A, B, C, D) => E): Par[E] = {
    val ab = map2(pa, pb)((_, _))
    val cd = map2(pc, pd)((_, _))
    map2(ab, cd) { case ((a, b), (c, d)) => f(a, b, c, d) }
  }

  // exercise 7.6
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val ps = parMap(as)(a => if (f(a)) Some(a) else None)
    map(ps)(_.flatten)
  }

  // exercise 7.11
  def choiceViaChoiceN[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choiceN(map(cond)(b => if (b) 1 else 0))(List(f, t))

  // exercise 7.13
  def choiceViaChooser[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    chooser(cond)(b => if (b) t else f)

  // exercise 7.13
  def choiceNViaChooser[A](pn: Par[Int])(choices: List[Par[A]]): Par[A] =
    chooser(pn)(i => choices(i))

  // exercise 7.13
  def choiceMapViaChooser[K, V](pk: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    chooser(pk)(k => choices(k))

  // exercise 7.13
  def flatMap[A, B](pa: Par[A])(f: A => Par[B]): Par[B] =
    chooser(pa)(f)

  // exercise 7.11 - fails if (i < 0) || (i >= choice.size)
  def choiceN[A](pn: Par[Int])(choices: List[Par[A]]): Par[A] =
    es => {
      val n = run(es)(pn)
      choices(n)(es)
    }

  // exercise 7.12 - fails if key not in choices
  def choiceMap[K, V](pk: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    es => {
      val k = run(es)(pk)
      choices(k)(es)
    }

  // exercise 7.13
  def chooser[A, B](pa: Par[A])(f: A => Par[B]): Par[B] =
    es => {
      val a = run(es)(pa)
      f(a)(es)
    }

  // exercise 7.14
  def join[A](ppa: Par[Par[A]]): Par[A] =
    es => {
      val ap = run(es)(ppa)
      ap(es)
    }

  // exercise 7.14
  def joinViaFlatMap[A](ppa: Par[Par[A]]): Par[A] =
    flatMap(ppa)(identity)

  // exercise 7.14
  def flatMapViaJoin[A, B](pa: Par[A])(f: A => Par[B]): Par[B] =
    join(map(pa)(f))
}

object Examples {
  import Par._
  def sum(ints: IndexedSeq[Int]): Int = // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
    if (ints.size <= 1)
      ints.headOption getOrElse 0 // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
    else {
      val (l, r) = ints.splitAt(ints.length / 2) // Divide the sequence in half using the `splitAt` function.
      sum(l) + sum(r) // Recursively sum both halves and add the results together.
    }

}