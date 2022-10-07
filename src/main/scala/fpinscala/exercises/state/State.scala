package fpinscala.exercises.state

import scala.annotation.tailrec


trait RNG:
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.

object RNG:
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG:
    def nextInt: (Int, RNG) =
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) =
    val (i, newRng) = rng.nextInt
    val positiveInt = if i < 0 then -(i + 1) else i
    (positiveInt, newRng)

  def double(rng: RNG): (Double, RNG) =
    val (i, newRng) = nonNegativeInt(rng)
    val double = i / (Int.MaxValue.toDouble + 1)
    (double, newRng)

  val _double: Rand[Double] =
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  def intDouble(rng: RNG): ((Int,Double), RNG) =
    val (i, rng1) = rng.nextInt
    val (d, rng2) = double(rng1)
    ((i, d), rng2)

  def doubleInt(rng: RNG): ((Double,Int), RNG) =
    val ((i, d), newRng) = intDouble(rng)
    ((d, i), newRng)

  def double3(rng: RNG): ((Double,Double,Double), RNG) =
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)
    ((d1, d2, d3), rng3)

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    if count <= 0 then (List.empty, rng)
    else
      val (i, newRng) = rng.nextInt
      val (is, newRng2) = ints(count - 1)(newRng)
      (i :: is, newRng2)

  def ints2(count: Int)(rng: RNG): (List[Int], RNG) =
    @tailrec
    def loop(counter: Int, r: RNG, acc: List[Int]): (List[Int], RNG) =
      if counter <= 0 then (acc, r)
      else
        val (i, r2) = r.nextInt
        loop(counter - 1, r2, i :: acc)

    loop(count, rng, List.empty)

  def ints3(count: Int)(rng: RNG): (List[Int], RNG) =
    (1 to count).foldRight((List.empty[Int], rng)) { case (a, (acc, r)) =>
      val (i, r2) = r.nextInt
      (i :: acc, r2)
    }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng =>
      val (r1, rng1) = ra(rng)
      val (r2, rng2) = rb(rng1)
      (f(r1, r2), rng2)

  val intDoubleViaMap2: Rand[(Int, Double)] = map2(int, _double)((_, _))

  val doubleIntViaMap2: Rand[(Double, Int)] = map2(_double, int)((_, _))

  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] = ???

  def flatMap[A, B](r: Rand[A])(f: A => Rand[B]): Rand[B] = ???

  def mapViaFlatMap[A, B](r: Rand[A])(f: A => B): Rand[B] = ???

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = ???

  @main def testRng() = {
    val seed = 123456789L
    val rng = Simple(seed)

    println(s"intDouble       : ${intDouble(rng)}")
    println(s"intDoubleViaMap2: ${intDoubleViaMap2(rng)}")
    println()
    println(s"doubleInt       : ${doubleInt(rng)}")
    println(s"doubleIntViaMap2: ${doubleIntViaMap2(rng)}")

  }

opaque type State[S, +A] = S => (A, S)

object State:
  extension [S, A](underlying: State[S, A])
    def run(s: S): (A, S) = underlying(s)

    def map[B](f: A => B): State[S, B] =
      ???

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      ???

    def flatMap[B](f: A => State[S, B]): State[S, B] =
      ???

  def apply[S, A](f: S => (A, S)): State[S, A] = f

enum Input:
  case Coin, Turn

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy:
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
