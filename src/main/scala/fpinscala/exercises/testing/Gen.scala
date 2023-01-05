package fpinscala.exercises.testing

import fpinscala.exercises.parallelism.*
import fpinscala.exercises.parallelism.Par.Par
import fpinscala.exercises.state.*
import fpinscala.exercises.testing.Gen.*
import fpinscala.exercises.testing.Prop.*
import fpinscala.exercises.testing.Prop.Result.{Falsified, Passed}

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
  type TestCases = Int

  enum Result:
    case Passed
    case Falsified(failure: FailedCase, successes: SuccessCount)

    def isFalsified: Boolean = this match
      case Passed => false
      case Falsified(_, _) => true

  case class Prop(run: (TestCases, RNG) => Result)

  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = Prop {
    (n, rng) => randomLazyList(gen)(rng).zip(LazyList.from(0)).take(n).map {
      case (generatedInput, i) =>
        try
          if f(generatedInput) then Passed else Falsified(generatedInput.toString, i)
        catch
          case e: Exception => Falsified(buildMsg(generatedInput, e), i)
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def randomLazyList[A](g: Gen[A])(rng: RNG): LazyList[A] =
    LazyList.unfold(rng)(rng => Some(g.next(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

opaque type Gen[A] = State[RNG, A]

object Gen:
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    State(RNG.nonNegativeLessThan(stopExclusive - start)).map(_ + start)

  def unit[A](a: => A): Gen[A] = State.unit(a)

  val boolean: Gen[Boolean] = State(RNG.boolean)

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    State.traverse(Range(0, n).toList)(_ => g)
    
  def choose2(start: Int, stopExclusive: Int): Gen[(Int, Int)] = State { (rng: RNG) =>
      val (firstInt, newRng) = Gen.choose(start, stopExclusive).next(rng)
      val (secondInt, newRng2) = Gen.choose(start, stopExclusive).next(newRng)
      ((firstInt, secondInt), newRng2)
  }

  def fromOption[A](gen: Gen[Option[A]], orElse: => A): Gen[A] = gen.map(_.getOrElse(orElse))

  val char: Gen[Char] = {
    def intToChar(i: Int): Char = (if i == 34 then i + 1 else i).toChar

    Gen.choose(33, 127).map(intToChar)
  }

  def string(length: Int): Gen[String] = Gen.listOfN[Char](length, char).map(_.mkString)

  def union[A](gen1: Gen[A], gen2: Gen[A]): Gen[A] = Gen.boolean.flatMap {
    case true => gen1
    case false => gen2
  }
  
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] =
    val threshold = g1._2.abs / (g1._2.abs + g2._2.abs)
    State(RNG.double).flatMap(w => if w < threshold then g1._1 else g2._1)

  extension [A](self: Gen[A])
    def next(rng: RNG): (A, RNG) = self.run(rng)

    def toOption: Gen[Option[A]] = self.map(Some(_))

    def flatMap[B](f: A => Gen[B]): Gen[B] = State { (rng: RNG) =>
      val (value, newRng) = self.next(rng)
      f(value).next(newRng)
    }

    def map[B](f: A => B): Gen[B] = flatMap(a => Gen.unit(f(a)))

    def listOfN(size: Int): Gen[List[A]] = Gen.listOfN(size, self)

    def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(listOfN(_))

//trait Gen[A]:
//  def map[B](f: A => B): Gen[B] = ???
//  def flatMap[B](f: A => Gen[B]): Gen[B] = ???

trait SGen[+A]
