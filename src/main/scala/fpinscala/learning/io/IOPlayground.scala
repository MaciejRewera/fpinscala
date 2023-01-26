package fpinscala.learning.io

import fpinscala.exercises.iomonad.Monad

import scala.annotation.tailrec
import scala.io.StdIn.readLine

object IOPlayground {

  @main def main = {
    Factorial.factorialRepl.run
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
}
