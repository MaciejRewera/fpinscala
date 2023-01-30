package fpinscala.learning.io

import fpinscala.answers.iomonad.Free
import fpinscala.exercises.iomonad.Monad
import fpinscala.learning.io.Free.Return

import scala.annotation.tailrec
import scala.util.control.TailCalls
import fpinscala.learning.io.Free.given

enum Free[+F[_], A]:
  case Return(a: A) extends Free[Nothing, A]
  case Suspend(s: F[A])
  case FlatMap[F[_], A, B](s: Free[F, A], f: A => Free[F, B]) extends Free[F, B]

  def flatMap[F2[x] >: F[x], B](f: A => Free[F2, B]): Free[F2, B] = FlatMap(this, f)

  def map[B](f: A => B): Free[F, B] = this.flatMap(a => Return(f(a)))

  def covary[F2[x] >: F[x]]: Free[F2, A] = this

  def run[F2[x] >: F[x]](using fMonad: Monad[F2]): F2[A] = this.step match
    case Return(a) => fMonad.unit(a)
    case Suspend(fa) => fa
    case FlatMap(Suspend(fa), f) => fa.flatMap(a => f(a).run)
    case FlatMap(_, _) => sys.error("Impossible, since `step` eliminates these cases")

  @tailrec
  final def step: Free[F, A] = this match
    case FlatMap(FlatMap(fx, f), g) => fx.flatMap(x => f(x).flatMap(y => g(y).covary[F])).step
    case FlatMap(Return(x), f) => f(x).step
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

end Free