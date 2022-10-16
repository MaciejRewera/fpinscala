package fpinscala.learning.parallelism

import java.util.concurrent.{Callable, ExecutorService, Future}

type Par[A] = ExecutorService => Future[A]

object Par:

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone(): Boolean = true
    def get(timeout: Long, units: java.util.concurrent.TimeUnit): A = get
    def isCancelled(): Boolean = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def unit[A](a: A): Par[A] = (_: ExecutorService) => UnitFuture(a)

  def map2[A, B, C](parA: => Par[A], parB: => Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) =>
      val a = parA(es)
      val b = parB(es)
      UnitFuture(f(a.get, b.get))

  def fork[A](par: => Par[A]): Par[A] = (es: ExecutorService) =>
    es.submit(new Callable[A] {
      override def call(): A = par(es).get
    })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](es: ExecutorService)(par: Par[A]): Future[A] = ???
