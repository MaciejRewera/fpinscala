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


//opaque type Gen[A] = State[RNG, A]

case class Gen[A](sample: State[RNG, A]):
  self =>
  
  def next(rng: RNG): (A, RNG) = self.sample.run(rng)

  def toOption: Gen[Option[A]] = Gen(self.sample.map(Some(_)))

object Gen:
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeLessThan(stopExclusive - start)).map(_ + start))

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = Gen(State(RNG.boolean))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.traverse(Range(0, n).toList)(_ => g.sample))
    
  def choose2(start: Int, stopExclusive: Int): Gen[(Int, Int)] = Gen(State(
    (rng: RNG) =>
      val (firstInt, newRng) = Gen.choose(start, stopExclusive).next(rng)
      val (secondInt, newRng2) = Gen.choose(start, stopExclusive).next(newRng)
      ((firstInt, secondInt), newRng2)
  ))

  def fromOption[A](gen: Gen[Option[A]], orElse: => A): Gen[A] = Gen(gen.sample.map(_.getOrElse(orElse)))

  def char: Gen[Char] = {
    def intToChar(i: Int): Char = (if i == 34 then i + 1 else i).toChar

    Gen(Gen.choose(33, 127).sample.map(intToChar))
  }

  def string(length: Int): Gen[String] = Gen(Gen.listOfN[Char](length, char).sample.map(_.mkString))

  extension [A](self: Gen[A])
    def flatMap[B](f: A => Gen[B]): Gen[B] = ???

//trait Gen[A]:
//  def map[B](f: A => B): Gen[B] = ???
//  def flatMap[B](f: A => Gen[B]): Gen[B] = ???

trait SGen[+A]
