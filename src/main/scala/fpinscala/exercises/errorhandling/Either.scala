package fpinscala.exercises.errorhandling

// Hide std library `Either` since we are writing our own in this chapter
import scala.util.control.NonFatal
import scala.{Either as _, Left as _, Right as _}

enum Either[+E,+A]:
  case Left(get: E)
  case Right(get: A)

  def map[B](f: A => B): Either[E, B] = this match
    case Left(e) => Left(e)
    case Right(v) => Right(f(v))

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match
    case Left(e) => Left(e)
    case Right(v) => f(v)

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match
    case Left(_) => b
    case Right(v) => Right(v)

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      aa <- this
      bb <- b
    } yield f(aa, bb)

object Either:
  def traverse[E,A,B](list: List[A])(f: A => Either[E, B]): Either[E, List[B]] = list match
    case Nil => Right(Nil)
    case hd :: tl => f(hd).map2(traverse(tl)(f))(_ :: _)

  def traverse2[E,A,B](list: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    list.foldRight[Either[E, List[B]]](Right(List.empty)) { (x, acc) => f(x).map2(acc)(_ :: _) }

  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] = traverse(es)(identity)

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
    if xs.isEmpty then
      Left("mean of empty list!")
    else 
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Throwable, Int] = 
    try Right(x / y)
    catch case NonFatal(t) => Left(t)

  def catchNonFatal[A](a: => A): Either[Throwable, A] =
    try Right(a)
    catch case NonFatal(t) => Left(t)

  def map2All[E, A, B, C](a: Either[List[E], A], b: Either[List[E], B], f: (A, B) => C): Either[List[E], C] = ???

  def traverseAll[E, A, B](es: List[A], f: A => Either[List[E], B]): Either[List[E], List[B]] = ???

  def sequenceAll[E, A](es: List[Either[List[E], A]]): Either[List[E], List[A]] = ???

  @main def testEither = {

    println(s" traverse:  ${traverse(List("1", "2", "3"))(i => catchNonFatal(i + "ms"))}")
    println(s"traverse2:  ${traverse2(List("1", "2", "3"))(i => catchNonFatal(i + "ms"))}")
    println(s" traverse:  ${traverse(List("1", "q"))(i => catchNonFatal(i.toInt))}")
    println(s"traverse2:  ${traverse2(List("1", "q"))(i => catchNonFatal(i.toInt))}")

    println(s"sequence: ${sequence(List(Right(1), Right(2), Right(3)))}")
    println(s"sequence: ${sequence(List(Right(1), Left("Nope"), Right(3)))}")
  }