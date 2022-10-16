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

  def map2[A, B, C](parA: => Par[A], parB: => Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) =>
      val a = parA(es)
      val b = parB(es)
      UnitFuture(f(a.get, b.get))

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

  def fork[A](par: => Par[A]): Par[A] = (es: ExecutorService) =>
    es.submit(new Callable[A] {
      override def call(): A = par(es).get
    })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](es: ExecutorService)(par: Par[A]): Future[A] = ???

  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))
