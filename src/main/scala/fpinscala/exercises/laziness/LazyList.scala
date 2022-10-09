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
    case Cons(h, t) if n == 1 => cons(h(), empty)
    case Cons(h, t) if n > 1  => cons(h(), t().take(n - 1))
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

  def flatMap[B](f: A => LazyList[B]): LazyList[B] = foldRight(empty[B])((a, acc) => f(a).append(acc))

  def filter(p: A => Boolean): LazyList[A] = foldRight(empty)((a, acc) => if p(a) then cons(a, acc) else acc)

  def append[A2 >: A](other: => LazyList[A2]): LazyList[A2] = foldRight(other)(cons)

  def mapViaUnfold[B](f: A => B): LazyList[B] = LazyList.unfold(this) {
    case Cons(h, t) => Some((f(h()), t()))
    case Empty => None
  }

  def takeViaUnfold(n: Int): LazyList[A] = LazyList.unfold((n, this)) {
    case (1, Cons(h, t))              => Some(h() -> (0, empty))
    case (num, Cons(h, t)) if num > 1 => Some(h() -> (num - 1, t()))
    case _ => None
  }

  def takeWhileViaUnfold(p: A => Boolean): LazyList[A] = LazyList.unfold(this) {
    case Cons(h, t) if p(h()) => Some(h(), t())
    case _ => None
  }

  def zipWith[B, C](other: LazyList[B])(f: (A, B) => C): LazyList[C] = LazyList.unfold((this, other)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()) -> (t1(), t2()))
    case (_, _) => None
  }

  def zip[B](other: LazyList[B]): LazyList[(A, B)] = zipWith(other)((_, _))

  def zipAll[B](other: LazyList[B]): LazyList[(Option[A], Option[B])] = LazyList.unfold((this, other)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some(Some(h1()) -> Some(h2()), (t1(), t2()))
    case (Empty, Cons(h2, t2)) => Some(None -> Some(h2()), (Empty, t2()))
    case (Cons(h1, t1), Empty) => Some(Some(h1()) -> None, (t1(), Empty))
    case _ => None
  }

  def startsWith[A](sub: LazyList[A]): Boolean =
    this.zipAll(sub).takeWhile(_._2.isDefined).forAll { case (a1, a2) => a1 == a2 }

  def tails: LazyList[LazyList[A]] = LazyList.unfold(this) {
    case Cons(h, t) => Some(Cons(h, t) -> t())
    case Empty      => None
  }.append(LazyList(empty))

  def hasSubsequence[A](sub: LazyList[A]): Boolean = tails.exists(_.startsWith(sub))

  // This is the same solution as in answers, but they're both strict (!!!)
  def scanRight[B](z: B)(f: (A, => B) => B): LazyList[B] =
    foldRight((z, LazyList(z))) { (a, acc) =>
      lazy val (previousSingleResult, accumulatedResults) = acc

      val newSingleResult = f(a, previousSingleResult)
      (newSingleResult, cons(newSingleResult, accumulatedResults))
    }._2

  def scanRightRecomputing[B](z: => B)(f: (A, => B) => B): LazyList[B] =
    tails.map(_.foldRight(z)(f))


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

  def continually[A](a: A): LazyList[A] = cons(a, continually(a))

  def continually2[A](a: => A): LazyList[A] =
    lazy val single : LazyList[A] = cons(a, single)
    single

  def from(n: Int): LazyList[Int] = cons(n, from(n + 1))
  
  def from2(n: => Int): LazyList[Int] =
    lazy val num = n
    cons(num, from(num + 1))

  lazy val fibs: LazyList[Int] =
    def loop(current: Int, next: Int): LazyList[Int] =
      cons(current, loop(next, current + next))
    loop(0, 1)

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] = f(state) match
    case None => empty
    case Some((value, newState)) => cons(value, unfold(newState)(f))

  lazy val fibsViaUnfold: LazyList[Int] =
    unfold((0, 1)) { case (current, next) => Some(current, (next, current + next)) }

  def fromViaUnfold(n: Int): LazyList[Int] = unfold(n)(num => Some((num, num + 1)))

  def continuallyViaUnfold[A](a: A): LazyList[A] = unfold(())(_ => Some((a, ())))

  lazy val onesViaUnfold: LazyList[Int] = unfold(())(_ => Some((1, ())))

  def repeatedly[A](elems: A*): LazyList[A] =
    def loop(rest: A*): LazyList[A] =
      if rest.isEmpty then empty
      else if rest.tail.isEmpty then cons(rest.head, loop(elems*))
      else cons(rest.head, loop(rest.tail*))

    loop(elems*)

  @main def testLazyList(): Unit = {
    def gen(n: => Int): Int = {
      lazy val num = n
      println(s"Evaluating num: $num")
      num
    }

    val elements = Seq(gen(1), gen(2), gen(3), gen(4))
    val repeated = LazyList.repeatedly(elements*)

    println(s"repeatedly (empty): ${LazyList.repeatedly().take(10).toList}")
    println(s"repeatedly ('a'): ${LazyList.repeatedly('a').take(10).toList}")
    println(s"repeatedly (1,2,3): ${LazyList.repeatedly(1, 2, 3).take(10).toList}")
    println(s"repeatedly (123)  : ${repeated.take(123).toList}")
    repeated.take(101).toList.grouped(elements.length).foreach(println)
  }