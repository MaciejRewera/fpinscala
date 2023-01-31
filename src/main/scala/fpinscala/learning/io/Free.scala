package fpinscala.learning.io

import fpinscala.answers.iomonad.Free
import fpinscala.answers.parallelism.Nonblocking.Par
import fpinscala.exercises.iomonad.Monad
import fpinscala.exercises.iomonad.Monad.given Monad[Par]
import fpinscala.exercises.monoids.Monoid
import fpinscala.exercises.monoids.Monoid.intAddition
import fpinscala.learning.io.Free.Console.{ConsoleIO, consoleToPar, consoleToThunk, ~>}
import fpinscala.learning.io.Free.{Return, given}

import java.util.concurrent.{ExecutorService, Executors}
import scala.annotation.tailrec
import scala.concurrent.ExecutionContext
import scala.io.StdIn.readLine
import scala.util.Try
import scala.util.control.TailCalls

enum Free[F[_], A]:
  case Return(a: A)
  case Suspend(s: F[A])
  case FlatMap[F[_], A, B](s: Free[F, A], f: A => Free[F, B]) extends Free[F, B]

  def flatMap[B](f: A => Free[F, B]): Free[F, B] = FlatMap(this, f)

  def map[B](f: A => B): Free[F, B] = this.flatMap(a => Return(f(a)))

  def run(using fMonad: Monad[F]): F[A] = this.step match
    case Return(a) => fMonad.unit(a)
    case Suspend(fa) => fa
    case FlatMap(Suspend(fa), f) => fa.flatMap(a => f(a).run)
    case FlatMap(_, _) => sys.error("Impossible, since `step` eliminates these cases")

  @tailrec
  final def step: Free[F, A] = this match
    case FlatMap(FlatMap(fx, f), g) => fx.flatMap(x => f(x).flatMap(y => g(y))).step
    case FlatMap(Return(x), f) => f(x).step
    case _ => this

  def runFree[G[_]](t: F ~> G)(using gMonad: Monad[G]): G[A] = this.step match
    case Return(a) => gMonad.unit(a)
    case Suspend(fa) => t(fa)
    case FlatMap(Suspend(fa), f) => t(fa).flatMap(a => f(a).runFree(t))
    case FlatMap(_, _) => sys.error("Impossible, since `step` eliminates these cases")
//
//  def runFree[G[_]](t: [x] => F[x] => G[x])(using gMonad: Monad[G]): G[A] = this.step match
//    case Return(a) => gMonad.unit(a)
//    case Suspend(fa) => t(fa)
//    case FlatMap(Suspend(fa), f) => t(fa).flatMap(a => f(a).runFree(t))
//    case FlatMap(_, _) => sys.error("Impossible, since `step` eliminates these cases")

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

  enum Console[A]:
    case ReadLine extends Console[Option[String]]
    case PrintLine(line: String) extends Console[Unit]

    def toPar: Par[A] = this match
      case ReadLine => Par.lazyUnit(Try(readLine()).toOption)
      case PrintLine(line) => Par.lazyUnit(println(line))

    def toThunk: () => A = this match
      case ReadLine => () => Try(readLine()).toOption
      case PrintLine(line) => () => println(line)

  end Console

  object Console:
    type ConsoleIO[A] = Free[Console, A]

    def readLn: ConsoleIO[Option[String]] = Suspend(ReadLine)

    def printLn(line: String): ConsoleIO[Unit] = Suspend(PrintLine(line))

    trait Translate[F[_], G[_]] { def apply[A](f: F[A]): G[A]}
    type ~>[F[_], G[_]] = Translate[F, G]

    val consoleToThunk: Console ~> Function0 = new (Console ~> Function0) {
      override def apply[A](f: Console[A]): () => A = f.toThunk
    }
    val consoleToPar: Console ~> Par = new (Console ~> Par) {
      override def apply[A](f: Console[A]): Par[A] = f.toPar
    }

    def runConsoleFunction0[A](a: ConsoleIO[A]): () => A =
      a.runFree[Function0](consoleToThunk)

    def runConsolePar[A](a: ConsoleIO[A]): Par[A] =
      a.runFree[Par](consoleToPar)

    extension [A](fa: Free[Console, A])
      def toThunk: () => A = fa.runFree(consoleToThunk)
      def toPar: Par[A] = fa.runFree(consoleToPar)

  end Console

  @main def testConsole: Unit = {
    val program: ConsoleIO[Option[String]] = for {
      _ <- Console.printLn("Hello, World!")
      res <- Console.readLn
    } yield (res)

    val es = Executors.newFixedThreadPool(4)
    given Monad[Par] with
      def unit[A](a: => A) = Par.unit(a)
      extension[A] (fa: Par[A])
        def flatMap[B](f: A => Par[B]) = Par.fork(Par.flatMap(fa)(f))

//    Console.runConsolePar(program).run(es)
    program.toPar.run(es)

    given Monad[Function0] with
      def unit[A](a: => A) = () => a
      extension [A](fa: Function0[A])
        def flatMap[B](f: A => Function0[B]) = () => f(fa())()

//    Console.runConsoleFunction0(program)()
    program.toThunk.apply()
  }

end Free






















