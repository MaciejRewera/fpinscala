package fpinscala.learning.io

import fpinscala.answers.iomonad.Free

enum Free[+F[_], A]:
  case Return(a: A) extends Free[Nothing, A]
  case Suspend(s: F[A])
  case FlatMap[F[_], A, B](s: Free[F, A], f: A => Free[F, B]) extends Free[F, B]

  def flatMap[F2[x] >: F[x], B](f: A => Free[F2, B]): Free[F2, B] = FlatMap(this, f)

  def map[B](f: A => B): Free[F, B] = this.flatMap(a => Return(f(a)))

  @tailrec
  def step: Free[F, A] = this match
    case FlatMap(FlatMap(x, f), g) => step(x.flatMap(f).flatMap(g))
    case FlatMap(Return(x), f) => step(f(x))
    case _ => this


end Free

object Free:
  given freeMonad[F[_]]: Monad[[a] =>> Free[F, a]] with
    def unit[A](a: => A): Free[F, A] = Return(a)
    extension [A](fa: Free[F, A])
      override def flatMap[B](f: A => Free[F, B]): Free[F, B] = fa.flatMap(f)

  extension[A] (fa: Free[Function0, A])
    @tailrec
    def runTrampoline: A = fa match
      case Return(a) => a
      case Suspend(s) => s()
      case FlatMap(x, f) => x match
        case Return(a) => f(a).runTrampoline
        case Suspend(s) => f(s()).runTrampoline
        case FlatMap(y, g) => y.flatMap(g).flatMap(f).runTrampoline
