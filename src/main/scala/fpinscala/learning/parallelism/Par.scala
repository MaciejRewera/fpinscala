package fpinscala.learning.parallelism

import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}

type Par[A] = ExecutorService => Future[A]

object Par:

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone(): Boolean = true
    def get(timeout: Long, units: TimeUnit): A = get
    def isCancelled(): Boolean = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def unit[A](a: A): Par[A] = (_: ExecutorService) => UnitFuture(a)

  def map2[A, B, R](parA: => Par[A], parB: => Par[B])(f: (A, B) => R): Par[R] =
    (es: ExecutorService) =>
      val a = parA(es)
      val b = parB(es)
      UnitFuture(f(a.get, b.get))

  def flatMap2[A, B, R](parA: => Par[A], parB: => Par[B])(f: (A, B) => Par[R]): Par[R] =
    (es: ExecutorService) =>
      val a = parA(es)
      val b = parB(es)
      f(a.get, b.get)(es)

  def map3[A, B, C, R](parA: => Par[A], parB: => Par[B], parC: => Par[C])(f: (A, B, C) => R): Par[R] =
    for {
      a <- parA
      b <- parB
      c <- parC
    } yield f(a, b, c)

  def map4[A, B, C, D, R](parA: => Par[A], parB: => Par[B], parC: => Par[C], parD: => Par[D])(
    f: (A, B, C, D) => R
  ): Par[R] = for {
    a <- parA
    b <- parB
    c <- parC
    d <- parD
  } yield f(a, b, c, d)

  def map5[A, B, C, D, E, R](parA: => Par[A], parB: => Par[B], parC: => Par[C], parD: => Par[D], parE: => Par[E])(
    f: (A, B, C, D, E) => R
  ): Par[R] = for {
    a <- parA
    b <- parB
    c <- parC
    d <- parD
    e <- parE
  } yield f(a, b, c, d, e)

  def map2Timeouts[A, B, C](parA: => Par[A], parB: => Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => new Future[C] {
      private val futureA = parA(es)
      private val futureB = parB(es)
      @volatile private var result: Option[C] = None

      def isDone(): Boolean = result.isDefined

      def get(): C = get(Long.MaxValue, TimeUnit.NANOSECONDS)

      def get(timeout: Long, units: TimeUnit): C =
        val timeoutInNanos = TimeUnit.NANOSECONDS.convert(timeout, units)
        val startTime = System.nanoTime()
        val resA = futureA.get(timeoutInNanos, TimeUnit.NANOSECONDS)
        val middleTime = System.nanoTime()
        val timeLeftForFutureB = timeoutInNanos - (middleTime - startTime)
        val resB = futureB.get(timeLeftForFutureB, TimeUnit.NANOSECONDS)

        val res = f(resA, resB)
        result = Some(res)
        res

      def isCancelled(): Boolean = futureA.isCancelled || futureB.isCancelled

      def cancel(evenIfRunning: Boolean): Boolean =
        futureA.cancel(evenIfRunning) || futureB.cancel(evenIfRunning)
    }

  extension [A](par: Par[A]) def map[B](f: A => B): Par[B] = map2(par, unit(()))((a, _) => f(a))

  extension [A](par: Par[A]) def flatMap[B](f: A => Par[B]): Par[B] = flatMap2(par, unit(()))((a, _) => f(a))

  def fork[A](par: => Par[A]): Par[A] = (es: ExecutorService) =>
    es.submit(new Callable[A] {
      override def call(): A = par(es).get
    })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  extension [A](par: Par[A]) def run(es: ExecutorService): Future[A] = par(es)

  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))
  
  def equal[A](es: ExecutorService)(p1: Par[A], p2: Par[A]): Boolean =
    p1(es).get == p2(es).get

  def sortPar[A](parList: Par[Seq[A]])(implicit ord: Ordering[A]): Par[Seq[A]] =
    parList.map(_.sorted(ord))

  def parMap[A, B](list: Seq[A])(f: A => B): Par[Seq[B]] = fork {
    list.map(asyncF(f)).toIndexedSeq.sequenceBalanced.map(_.toSeq)
  }

  def parMapSlow[A, B](list: Seq[A])(f: A => B): Par[Seq[B]] =
    list.map(asyncF(f)).sequenceSimple

  extension [A](list: Seq[Par[A]]) def sequenceSimple: Par[Seq[A]] =
    list.foldRight(unit(Seq.empty[A]))((par, acc) => map2(par, acc)(_ +: _))

  extension [A](list: IndexedSeq[Par[A]]) def sequenceBalanced: Par[IndexedSeq[A]] = list.traverse(identity)

  def parFilter[A](list: Seq[A])(f: A => Boolean): Par[Seq[A]] = fork {
    list.toIndexedSeq.traverse(a => unit(if f(a) then Seq(a) else Seq.empty)).map(_.flatten)
  }

  extension [A](list: IndexedSeq[A]) def traverse[B](f: A => Par[B]): Par[IndexedSeq[B]] =
    if (list.isEmpty) unit(IndexedSeq.empty)
    else if (list.length == 1)
      f(list.head).map(IndexedSeq(_))
    else
      val (l, r) = list.splitAt(list.length / 2)
      map2(l.traverse(f), r.traverse(f))(_ ++ _)

  def max(list: IndexedSeq[Int]): Par[Option[Int]] =
    list.parFoldMap(None)(Some(_)) { (optA1, optA2) => for { a1 <- optA1; a2 <- optA2 } yield Math.max(a1, a2) }

  def sum(list: IndexedSeq[Int]): Par[Int] =
    list.parFoldMap(0)(identity)(_ + _)

  extension [A](list: IndexedSeq[A]) def parFoldMap[B](z: B)(f: A => B)(assocF: (B, B) => B): Par[B] =
    if (list.isEmpty) unit(z)
    else if (list.length == 1) unit(f(list.head))
    else
      val (l, r) = list.splitAt(list.length / 2)
      map2(
        l.parFoldMap(z)(f)(assocF),
        r.parFoldMap(z)(f)(assocF)
      )(assocF)

  def countParagraphs(paragraphs: List[String]): Par[Int] =
    paragraphs.toIndexedSeq.parFoldMap(0)(_.split(' ').length)(_ + _)

  def choice[A](cons: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = es =>
    cons.run(es).get match {
      case true => t(es)
      case false => f(es)
    }

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = es =>
    val index = n.run(es).get % choices.size
    choices(index)(es)

  def choiceViaChoiceN[A](cons: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choiceN(cons.map { b => if b then 0 else 1 })(List(t, f))

  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] = es =>
    val k = key.run(es).get
    choices(k).run(es)
