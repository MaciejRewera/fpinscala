package fpinscala.learning.io

import fpinscala.exercises.iomonad.Monad
import fpinscala.learning.io.IOPlayground.TailRec.*

import scala.annotation.tailrec
import scala.io.StdIn.readLine
import scala.util.control.TailCalls

object IOPlayground {

  @main def main = {
//    TailRec.Suspend(() => println("Still going...")).forever.run

    val f: Int => TailRec[Int] = (x: Int) => Return(x)
    val g = List.fill(100000)(f).foldLeft(f) { (a, b) =>
      x => Return(x).flatMap(a).flatMap(b)
    }

    println(g(42).run)
    BuiltInTailRec.testRun(42).result
  }

  private val echo: IO[Unit] = ReadLine.flatMap(PrintLine)
  private val readInt: IO[Int] = ReadLine.map(_.toInt)
  private val readInts: IO[(Int, Int)] = readInt.map2(readInt)((_, _))
  private val readAndPrint: IO[Unit] = ReadLine.replicateM(3).flatMap(list => PrintLine(list.mkString))

  private def ReadLine: IO[String] = IO(readLine)
  private def PrintLine(msg: String): IO[Unit] = IO(println(msg))

  private def converter: IO[Unit] = for {
    _ <- PrintLine("Enter a temperature in degrees Fahrenheit")
    f <- ReadLine.map(_.toDouble)
    c = fahrenheitToCelsius(f)
    _ <- PrintLine(s"$f degrees fahrenheit is equal to $c degrees celsius")
  } yield ()

  private def fahrenheitToCelsius(f: Double): Double = (f - 32) * 5.0/9.0

  object Factorial {
    final val HelpString =
      """
        |The Amazing Factorial REPL, v1.0
        |q - quit
        |<number> - compute the factorial of the given number
        |<anything else> - crash spectacularly
        |""".stripMargin

    private def factorial(num: Int): IO[Int] =
      @tailrec
      def factorial(acc: Int, n: Int): Int = n match
        case 0 => acc
        case _ => factorial(acc * n, n - 1)

      IO(factorial(1, num))

    def factorialRepl: IO[Unit] = IO.ioMonad.sequence_(
      PrintLine(HelpString),
      ReadLine.doWhile { line =>
        IO.ioMonad.when(line != "q") { for {
          n <- factorial(line.toInt)
          _ <- PrintLine(s"factorial for $line is: $n")
        } yield () }
      }
    )

  }

  sealed trait IO[A] { self =>
    def run: A
    def map[B](f: A => B): IO[B] = new IO { def run = f(self.run) }
    def flatMap[B](f: A => IO[B]): IO[B] = new IO { def run = f(self.run).run }
  }

  object IO {
    def apply[A](a: => A)(using ioMonad: Monad[IO]): IO[A] = ioMonad.unit(a)

    given ioMonad: Monad[IO] with
      override def unit[A](a: => A): IO[A] = new IO { def run = a }
      extension[A] (fa: IO[A])
        override def flatMap[B](f: A => IO[B]): IO[B] = fa.flatMap(f)
  }
  
  enum TailRec[A]:
    case Return(a: A)
    case Suspend(resume: () => A)
    case FlatMap[A, B](sub: TailRec[A], k: A => TailRec[B]) extends TailRec[B]

    def flatMap[B](f: A => TailRec[B]): TailRec[B] = FlatMap(this, f)

    def map[B](f: A => B): TailRec[B] = flatMap(a => Return(f(a)))

    @tailrec
    final def run: A = this match
      case Return(a) => a
      case Suspend(r) => r()
      case FlatMap(x, f) => x match
        case Return(a) => f(a).run
        case Suspend(r) => f(r()).run
        case FlatMap(y, g) => y.flatMap(a => g(a).flatMap(f)).run
//        case FlatMap(y, g) => FlatMap(y, a => FlatMap(g(a), f)).run
  end TailRec

  object TailRec:
    def apply[A](a: => A)(using tailRecMonad: Monad[TailRec]): TailRec[A] =
      Suspend(() => Return(a)).flatMap(identity)

    given tailRecMonad: Monad[TailRec] with
      override def unit[A](a: => A): TailRec[A] = TailRec(a)

      extension[A] (fa: TailRec[A])
        override def flatMap[B](f: A => TailRec[B]): TailRec[B] = fa.flatMap(f)
  end TailRec

  object BuiltInTailRec:
    private val f: Int => TailCalls.TailRec[Int] = (x: Int) => TailCalls.done(x)
    private val g = List.fill(100000)(f).foldLeft(f) { (a, b) =>
      x => TailCalls.tailcall(a(x).flatMap(b))
    }

    def testRun(n: Int): TailCalls.TailRec[Unit] = TailCalls.done(println(g(n).result))

  end BuiltInTailRec
}
