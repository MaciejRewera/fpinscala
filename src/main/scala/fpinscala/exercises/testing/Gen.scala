package fpinscala.exercises.testing

import fpinscala.exercises.parallelism.*
import fpinscala.exercises.parallelism.Par.Par
import fpinscala.exercises.state.*
import fpinscala.exercises.testing.Gen.*
import fpinscala.exercises.testing.Prop.*

import java.util.concurrent.{ExecutorService, Executors}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

trait Prop:
  self =>

  def check: Either[(FailedCase, SuccessCount), SuccessCount]

  def &&(other: Prop): Prop = new Prop {
    override def check: Either[(FailedCase, SuccessCount), SuccessCount] =
      self.check flatMap (_ => other.check)
  }

object Prop:
  type FailedCase = String
  type SuccessCount = Int

  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???


opaque type Gen[A] = State[RNG, A]

object Gen:
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    State(RNG.nonNegativeLessThan(stopExclusive - start)).map(_ + start)

  def unit[A](a: => A): Gen[A] = State.unit(a)

  def boolean: Gen[Boolean] = State(RNG.boolean)

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    State.traverse(Range(0, n).toList)(_ => g)
    
  def choose2(start: Int, stopExclusive: Int): Gen[(Int, Int)] = State { (rng: RNG) =>
      val (firstInt, newRng) = Gen.choose(start, stopExclusive).next(rng)
      val (secondInt, newRng2) = Gen.choose(start, stopExclusive).next(newRng)
      ((firstInt, secondInt), newRng2)
  }

  def fromOption[A](gen: Gen[Option[A]], orElse: => A): Gen[A] = gen.map(_.getOrElse(orElse))

  def char: Gen[Char] = {
    def intToChar(i: Int): Char = (if i == 34 then i + 1 else i).toChar

    Gen.choose(33, 127).map(intToChar)
  }

  def string(length: Int): Gen[String] = Gen.listOfN[Char](length, char).map(_.mkString)

  extension [A](self: Gen[A])
    def next(rng: RNG): (A, RNG) = self.run(rng)

    def toOption: Gen[Option[A]] = self.map(Some(_))

    def flatMap[B](f: A => Gen[B]): Gen[B] = State { (rng: RNG) =>
      val (value, newRng) = self.next(rng)
      f(value).next(newRng)
    }

    def map[B](f: A => B): Gen[B] = flatMap(a => Gen.unit(f(a)))

    def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(Gen.listOfN(_, self))

//trait Gen[A]:
//  def map[B](f: A => B): Gen[B] = ???
//  def flatMap[B](f: A => Gen[B]): Gen[B] = ???

trait SGen[+A]
