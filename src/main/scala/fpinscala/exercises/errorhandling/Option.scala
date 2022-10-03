package fpinscala.exercises.errorhandling

// Hide std library `Option` since we are writing our own in this chapter
import fpinscala.exercises.errorhandling.Option.mean

import scala.{None as _, Option as _, Some as _}

enum Option[+A]:
  case Some(get: A)
  case None

  def map[B](f: A => B): Option[B] = this match
    case None => None
    case Some(v) => Some(f(v))

  def getOrElse[B>:A](default: => B): B = this match
    case None => default
    case Some(v) => v

  def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)

  def orElse[B>:A](ob: => Option[B]): Option[B] = map(Some(_)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] = flatMap(a => if f(a) then Some(a) else None)

  def flatten: Option[A] = this match
    case Some(a: Option[A]) => a
    case o => o

object Option:

  def failingFn(i: Int): Int =
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try
      val x = 42 + 5
      x + y
    catch case e: Exception => 43 // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.

  def failingFn2(i: Int): Int =
    try
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    catch case e: Exception => 43

  def mean(xs: Seq[Double]): Option[Double] =
    if xs.isEmpty then None
    else Some(xs.sum / xs.length)

  extension (xs: Seq[Double]) def variance: Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  def map2[A,B,C](aOpt: Option[A], bOpt: Option[B])(f: (A, B) => C): Option[C] =
    for {
      a <- aOpt
      b <- bOpt
    } yield f(a, b)

  def sequence[A](as: List[Option[A]]): Option[List[A]] =
    as.foldRight[Option[List[A]]](Some(List.empty)) { (a, acc) => map2(a, acc)(_ :: _) }

  def sequence2[A](as: List[Option[A]]): Option[List[A]] = as match
    case Nil => Some(Nil)
    case hd :: tl => map2(hd, sequence2(tl))(_ :: _)

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None }

  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as.foldRight[Option[List[B]]](Some(Nil)) { (a, acc) => map2(f(a), acc)(_ :: _) }

  def traverse2[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = as match
    case Nil => Some(Nil)
    case hd :: tl => map2(f(hd), traverse2(tl)(f))(_ :: _)

  def sequenceViaTraverse[A](as: List[Option[A]]): Option[List[A]] = traverse(as)(identity)

  @main def testOption = {
    val list = List("1", "2", "3")

    println(s"traverse:  ${traverse(list)(i => Try(i + "ms"))}")
    println(s"traverse2: ${traverse2(list)(i => Try(i + "ms"))}")
    println(s"traverse:  ${traverse(List("1", "q"))(i => Try(i.toInt))}")
    println(s"traverse2: ${traverse2(List("1", "q"))(i => Try(i.toInt))}")

    println(s"sequenceViaTraverse: ${sequenceViaTraverse(List(Some(1), Some(2), Some(3)))}")
    println(s"sequenceViaTraverse: ${sequenceViaTraverse(List(Some(1), None, Some(3)))}")

    println(s"flatten (None): ${None.flatten}")
    println(s"flatten (Some(1)): ${Some(1).flatten}")
    println(s"flatten (Some(None)): ${Some(None).flatten}")
    println(s"flatten (Some(Some(1))): ${Some(Some(1)).flatten}")
    println(s"flatten (Some(Some(Some(1)))): ${Some(Some(Some(1))).flatten}")
    println(s"2xflatten (Some(Some(Some(1)))): ${Some(Some(Some(1))).flatten.flatten}")
  }