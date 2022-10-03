package fpinscala.exercises.laziness

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  def toListRecursive: List[A] = this match
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList

  def toList: List[A] =
    @tailrec
    def loop(rest: LazyList[A], acc: List[A]): List[A] = rest match
      case Cons(hd, tl) => loop(tl(), hd() :: acc)
      case Empty => acc.reverse

    loop(this, List.empty)

  def toListQuick: List[A] =
    val buffer = ListBuffer.empty[A]
    @tailrec
    def loop(rest: LazyList[A]): List[A] = rest match
      case Cons(hd, tl) =>
        buffer += hd()
        loop(tl())
      case Empty => buffer.toList

    loop(this)

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z

  @tailrec
  final def foldLeft[B](z: => B)(f: (B, A) => B): B =this match
      case Cons(h, t) => t().foldLeft(f(z, h()))(f)
      case _ => z

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the lazy list. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  def existsLeft(p: A => Boolean): Boolean = this match
    case Empty => false
    case Cons(h, t) => if p(h()) then true else t().exists(p)

  @tailrec
  final def existsRec(p: A => Boolean): Boolean = this match
    case Empty => false
    case Cons(hd, tl) => p(hd()) || tl().existsRec(p)

  @tailrec
  final def find(f: A => Boolean): Option[A] = this match
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)

  def take(n: Int): LazyList[A] = ???

  def drop(n: Int): LazyList[A] = ???

  def takeWhile(p: A => Boolean): LazyList[A] = ???

  def forAll(p: A => Boolean): Boolean = ???

  def headOption: Option[A] = ???

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def startsWith[B](s: LazyList[B]): Boolean = ???


object LazyList:
  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] =
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty
    else cons(as.head, apply(as.tail*))

  val ones: LazyList[Int] = LazyList.cons(1, ones)

  def continually[A](a: A): LazyList[A] = ???

  def from(n: Int): LazyList[Int] = ???

  lazy val fibs: LazyList[Int] = ???

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] = ???

  lazy val fibsViaUnfold: LazyList[Int] = ???

  def fromViaUnfold(n: Int): LazyList[Int] = ???

  def continuallyViaUnfold[A](a: A): LazyList[A] = ???

  lazy val onesViaUnfold: LazyList[Int] = ???

  @main def testLazyList(): Unit = {
    val stream = LazyList(1, 1, 2, 3, 3)

    val func = (a: Int) => {
      println(s"hi $a")
      val res = a % 2 == 0
      res
    }

    val funcLeft: (Int, Int) => Int = (acc, a) => {
      println(s"hi $a")
      val res = a + acc
      println(s"hi $a : ${res - a}")
      res
    }
    val funcRight: (Int, => Int) => Int = (a, acc) => {
      println(s"hi $a")
      val res = a + acc
      println(s"hi $a : ${res - a}")
      res
    }

    println(s"stream: ${stream.toList}")
    println(s"exists: ${stream.exists(func)}")
    println(s"existsRec: ${stream.existsRec(func)}")
    println(s"existsLeft: ${stream.existsLeft(func)}")
    println(s"foldLeft: ${stream.foldLeft(0)(funcLeft)}")
    println(s"foldRight: ${stream.foldRight(0)(funcRight)}")

  }