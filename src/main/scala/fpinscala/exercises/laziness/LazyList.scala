package fpinscala.exercises.laziness

import fpinscala.exercises.laziness.LazyList.{cons, empty}

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
    def loop(acc: List[A], rest: LazyList[A]): List[A] = rest match
      case Cons(hd, tl) => loop(hd() :: acc, tl())
      case Empty => acc.reverse

    loop(List.empty, this)

  def toListQuick: List[A] =
    val buffer = ListBuffer.empty[A]
    @tailrec
    def loop(rest: LazyList[A]): List[A] = rest match
      case Cons(hd, tl) =>
        buffer += hd()
        loop(tl())
      case Empty => buffer.toList

    loop(this)

  def take(n: Int): LazyList[A] = this match
    case Cons(h, t) if n > 0 => cons(h(), t().take(n - 1))
    case _ => empty

  def takeWhile(p: A => Boolean): LazyList[A] = this match
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty

  def takeWhileViaFoldRight(p: A => Boolean): LazyList[A] =
    foldRight(empty)((a, acc) => if p(a) then cons(a, acc) else empty)

  @tailrec
  final def drop(n: Int): LazyList[A] = this match
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the lazy list. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @tailrec
  final def existsRec(p: A => Boolean): Boolean = this match
    case Empty => false
    case Cons(hd, tl) => p(hd()) || tl().existsRec(p)

  @tailrec
  final def find(f: A => Boolean): Option[A] = this match
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)

  @tailrec
  final def forAll(p: A => Boolean): Boolean = this match
    case Cons(h, t) => if !p(h()) then false else t().forAll(p)
    case Empty => true

  final def forAllViaFoldRight(p: A => Boolean): Boolean =
    foldRight(true)((a, res) => p(a) && res)

  def headOptionRecursive: Option[A] = this match
    case Cons(h, t) => Some(h())
    case _ => None

  def headOption: Option[A] = foldRight(Option.empty[A])((a, _) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def mapRecursive[B](f: A => B): LazyList[B] = this match
    case Cons(h, t) => cons(f(h()), t().map(f))
    case Empty => empty

  def map[B](f: A => B): LazyList[B] = foldRight(empty)((a, acc) => cons(f(a), acc))

  def filter(p: A => Boolean): LazyList[A] = foldRight(empty)((a, acc) => if p(a) then cons(a, acc) else acc)

  def append[A2 >: A](other: => LazyList[A2]): LazyList[A2] = foldRight(other)(cons)

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
    def func(n: => Int): Int = {
      lazy val num = n
      println(s"Evaluating num: $num")
      num
    }
    val isOdd = (_: Int) % 2 != 0
    val isEven = (_: Int) % 2 == 0

    val stream: LazyList[Int] = cons(func(1), cons(func(2), cons(func(3), cons(func(4), cons(func(5), empty)))))
    val stream2: LazyList[Int] = cons(func(11), cons(func(12), cons(func(13), cons(func(14), cons(func(15), empty)))))

    println()
    println(s"append: ${stream.append(stream2)}")
    println(s"append(rev): ${stream2.append(stream)}")
    println(s"append(self): ${stream.append(stream)}")

    println(s"append.toList: ${stream.append(stream2).toList}")
    println(s"append(rev).toList: ${stream2.append(stream).toList}")
    println(s"append(self).toList: ${stream.append(stream).toList}")

  }