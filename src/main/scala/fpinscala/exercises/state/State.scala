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

  def intsViaSequence(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng =>
      val (r1, rng1) = ra(rng)
      val (r2, rng2) = rb(rng1)
      (f(r1, r2), rng2)

  val intDoubleViaMap2: Rand[(Int, Double)] = map2(int, _double)((_, _))

  val doubleIntViaMap2: Rand[(Double, Int)] = map2(_double, int)((_, _))

  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] =
    rs.foldRight(unit(List.empty[A]))((a, acc) => map2(a, acc)(_ :: _))

  private def tryAgainCondition(n: Int): Int => Boolean = i => (i >= Int.MaxValue - (Int.MaxValue % n))

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (tryAgainCondition(n)(i)) nonNegativeLessThan(n)
      else unit(mod)
    }

  def flatMap[A, B](r: Rand[A])(f: A => Rand[B]): Rand[B] = rng =>
    val (a, rng2) = r(rng)
    f(a)(rng2)

  def mapViaFlatMap[A, B](r: Rand[A])(f: A => B): Rand[B] = flatMap(r)(a => unit(f(a)))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => flatMap(rb)(b => unit(f(a, b))))

  def createDie(k: Int): Rand[Int] = map(nonNegativeLessThan(k))(_ + 1)

opaque type State[S, +A] = S => (A, S)

object State:
  extension [S, A](underlying: State[S, A])
    def run(s: S): (A, S) = underlying(s)

    def map[B](f: A => B): State[S, B] = flatMap(a => unit(f(a)))

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      for {
        a <- run
        b <- sb.run
      } yield f(a, b)

    def flatMap[B](f: A => State[S, B]): State[S, B] = state =>
      val (value, newState) = run(state)
      f(value)(newState)

  def apply[S, A](f: S => (A, S)): State[S, A] = f

  def unit[S, A](a: A): State[S, A] = state => (a, state)

  def sequence[S, A](rs: List[State[S, A]]): State[S, List[A]] =
    rs.foldRight[State[S, List[A]]](State.unit(List.empty)) { (state, acc) => state.map2(acc)(_ :: _) }

  def traverse[S, A, B](as: List[A])(f: A => State[S, B]): State[S, List[B]] =
    as.foldRight[State[S, List[B]]](State.unit(List.empty)) { (a, acc) => f(a).map2(acc)(_ :: _) }

  def get[S]: State[S, S] = s => (s, s)

  def set[S](s: S): State[S, Unit] = _ => ((), s)

  def modify[S](f: S => S): State[S, Unit] =
    get[S].flatMap(s => set[S](f(s)))

enum Input:
  case Coin, Turn

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy:
  def simulateMachine1(inputs: List[Input]): State[Machine, (Int, Int)] =
    State.apply { machine =>
      val resultMachine = inputs.foldLeft(machine) { (machineState, input) =>
        modifyMachine(input)(machineState)
      }

      ((resultMachine.coins, resultMachine.candies), resultMachine)
    }

  def simulateMachine2(inputs: List[Input]): State[Machine, (Int, Int)] =
      State.traverse(inputs)(i => State.modify(modifyMachine(i))).flatMap(_ => State.get).map(s => (s.coins, s.candies))

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    for {
      _ <- State.traverse(inputs)(i => State.modify(modifyMachine(i)))
      s <- State.get
    } yield (s.coins, s.candies)

  private def modifyMachine(input: Input) = (machine: Machine) =>
    (input, machine) match
      case (_, Machine(_, 0, _))                    => machine
      case (Input.Coin, Machine(false, _, _))       => machine
      case (Input.Turn, Machine(true, _, _))        => machine
      case (Input.Coin, Machine(true, _, coins))    => machine.copy(locked = false, coins = coins + 1)
      case (Input.Turn, Machine(false, candies, _)) => machine.copy(locked = true, candies = candies - 1)

  def simulateMachine4(inputs: List[Input]): State[Machine, (Int, Int)] =
    for {
      list <- State.traverse(inputs)(i => State.apply(update(i)))
      s <- State.get
    } yield list.lastOption.getOrElse((s.coins, s.candies))

  def simulateMachineAccumulate(inputs: List[Input]): State[Machine, List[(Int, Int)]] =
    for {
      list <- State.traverse(inputs)(i => State.apply(update(i)))
    } yield list

  private def update(input: Input) = (machine: Machine) =>
    (input, machine) match
      case (_, Machine(_, 0, _))                    => ((machine.coins, machine.candies), machine)
      case (Input.Coin, Machine(false, _, _))       => ((machine.coins, machine.candies), machine)
      case (Input.Turn, Machine(true, _, _))        => ((machine.coins, machine.candies), machine)
      case (Input.Coin, Machine(true, _, coins))    => {
        val newMachine = machine.copy(locked = false, coins = coins + 1)
        ((newMachine.coins, newMachine.candies), newMachine)
      }
      case (Input.Turn, Machine(false, candies, _)) => {
        val newMachine = machine.copy(locked = true, candies = candies - 1)
        ((newMachine.coins, newMachine.candies), newMachine)
      }

  @main def runCandyMachine = {
    val machine = Machine(true, 10, 1)
    val inputs: List[Input] = List(Input.Coin, Input.Turn, Input.Coin, Input.Turn, Input.Coin, Input.Turn, Input.Coin, Input.Coin)

    val (steps, newMachine) = simulateMachineAccumulate(inputs).run(machine)

    println(s"candies : ${steps.last._2}")
    println(s"coins   : ${steps.last._1}")
    println(s"newMachine.locked : ${newMachine.locked}")
    println(s"newMachine.candies: ${newMachine.candies}")
    println(s"newMachine.coins  : ${newMachine.coins}")
    println()
    println("steps (coins, candies):")
    println(s"${(machine.coins, machine.candies)}")
    println(s"${steps.mkString("\n")}")
  }
