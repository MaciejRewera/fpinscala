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

  def flatten: Either[E, A] = this match
    case Left(e: Either[E, A]) => e
    case Right(e: Either[E, A]) => e
    case e => e

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

  def sequence[E,A](list: List[Either[E,A]]): Either[E,List[A]] = traverse(list)(identity)

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

  def catchNonFatalAll[A](a: => A): Either[List[Throwable], A] =
    try Right(a)
    catch case NonFatal(t) => Left(List(t))

  def map2All[E, A, B, C](a: Either[List[E], A], b: Either[List[E], B])(f: (A, B) => C): Either[List[E], C] = (a, b) match
    case (Left(e1), Left(e2))   => Left(e1 ::: e2)
    case (Right(v1), Right(v2)) => Right(f(v1, v2))
    case (Left(e1), _)          => Left(e1)
    case (_, Left(e2))          => Left(e2)

  def traverseAll[E, A, B](list: List[A])(f: A => Either[List[E], B]): Either[List[E], List[B]] =
    list.foldRight[Either[List[E], List[B]]](Right(Nil)) { (x, acc) => map2All(f(x), acc)(_ :: _) }

  def sequenceAll[E, A](list: List[Either[List[E], A]]): Either[List[E], List[A]] = traverseAll(list)(identity)

  case class Person(name: Name, age: Age)
  object Person:
    def apply(name: String, age: Int): Either[String, Person] =
      Name(name).map2(Age(age))(Person(_, _))

  case class Name private (value: String)
  object Name:
    def apply(name: String): Either[String, Name] =
      if (name == "" || name == null) Left("Name is empty.")
      else Right(new Name(name))

  case class Age private (value: Int)
  object Age:
    def apply(age: Int): Either[String, Age] =
      if age < 0 then Left("Age is out of range.")
      else Right(new Age(age))

  def mkPerson(name: String, age: Int): Either[List[String], Person] =
    map2All(Name(name).liftToCumulative, Age(age).liftToCumulative)(Person(_, _))

  extension [E, A](e: Either[E, A]) def liftToCumulative: Either[List[E], A] = e match
    case Left(e) => Left(List(e))
    case Right(v) => Right(v)

  @main def testEither = {

    println(s"  Person: ${Person("Mike", 23)}")
    println(s"  Person: ${Person("", 23)}")
    println(s"  Person: ${Person("Mike", -1)}")
    println(s"  Person: ${Person("", -1)}")

    println(s"mkPerson: ${mkPerson("Mike", 23)}")
    println(s"mkPerson: ${mkPerson("", 23)}")
    println(s"mkPerson: ${mkPerson("Mike", -1)}")
    println(s"mkPerson: ${mkPerson("", -1)}")

    println()
    println(s"traverseAll: ${traverseAll(List("1", "2", "3"))(i => catchNonFatalAll(i + "ms"))}")
    println(s"traverseAll: ${traverseAll(List("1", "2", "3"))(i => catchNonFatalAll(i.toInt))}")
    println(s"traverseAll: ${traverseAll(List("1", "2", "w"))(i => catchNonFatalAll(i.toInt))}")
    println(s"traverseAll: ${traverseAll(List("1", "q", "w"))(i => catchNonFatalAll(i.toInt))}")

    println(s"sequenceAll: ${sequenceAll(List(Right(1), Right(2), Right(3)))}")
    println(s"sequenceAll: ${sequenceAll(List(Right("1"), Left(List("nope")), Right("3")))}")
    println(s"sequenceAll: ${sequenceAll(List(Right("1"), Left(List("nope")), Left(List("another nope"))))}")
  }