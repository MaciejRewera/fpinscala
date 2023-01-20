package fpinscala.exercises.monoids

import fpinscala.exercises.monoids.Monoid.WC.{Part, Stub}
import fpinscala.exercises.parallelism.Nonblocking.*

trait Monoid[A]:
  def combine(a1: A, a2: A): A
  def empty: A

object Monoid:

  val stringMonoid: Monoid[String] = new:
    def combine(a1: String, a2: String) = a1 + a2
    val empty = ""

  def listMonoid[A]: Monoid[List[A]] = new:
    def combine(a1: List[A], a2: List[A]) = a1 ++ a2
    val empty = Nil

  lazy val intAddition: Monoid[Int] = new:
    def combine(a1: Int, a2: Int): Int = a1 + a2
    val empty: Int = 0

  lazy val intMultiplication: Monoid[Int] = new:
    def combine(a1: Int, a2: Int): Int = a1 * a2
    val empty: Int = 1

  lazy val booleanOr: Monoid[Boolean] = new:
    def combine(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    val empty: Boolean = false

  lazy val booleanAnd: Monoid[Boolean] = new:
    def combine(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    val empty: Boolean = true

  def optionMonoid[A]: Monoid[Option[A]] = new:
    def combine(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2
    val empty: Option[A] = None

  def dual[A](m: Monoid[A]): Monoid[A] = new:
    def combine(x: A, y: A): A = m.combine(y, x)
    val empty: A = m.empty

  def endoMonoid[A]: Monoid[A => A] = new:
    def combine(a1: A => A, a2: A => A): A => A = a1 andThen a2
    val empty: A => A = a => a

  import fpinscala.exercises.testing.{Gen, Prop}
  // import Gen.`**`

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = ???

  def combineAll[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.empty)((a1, a2) => m.combine(a1, a2))

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.empty)((b, a) => m.combine(b, f(a)))

  def foldRight[A, B](as: List[A])(acc: B)(f: (A, B) => B): B =
    foldMap(as, dual(endoMonoid))(f.curried)(acc)

  def foldLeft[A, B](as: List[A])(acc: B)(f: (B, A) => B): B =
    foldMap(as, endoMonoid)(a => b => f(b, a))(acc)

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    if as.isEmpty then m.empty
    else if as.length == 1 then f(as.head)
    else
      val splitIdx = as.length / 2
      val (firstHalf, secondHalf) = as.splitAt(splitIdx)
      m.combine(foldMapV(firstHalf, m)(f), foldMapV(secondHalf, m)(f))

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new:
    def combine(p1: Par[A], p2: Par[A]): Par[A] = p1.map2(p2)(m.combine)
    val empty: Par[A] = Par.unit(m.empty)

  def parFoldMap[A,B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    Par.parMap(as)(f).flatMap { list =>
      foldMapV(list, par(m))(b => Par.delay(b))
    }

  case class Interval(ordered: Boolean, min: Int, max: Int)

  def orderingMonoid: Monoid[Option[Interval]] = new :
    def combine(oa1: Option[Interval], oa2: Option[Interval]): Option[Interval] =
      (oa1, oa2) match {
        case (Some(a1), Some(a2)) => Some(
          Interval(
            a1.ordered && a2.ordered && a1.max < a2.min,
            a1.min,
            a2.max
          )
        )
        case (a, None) => a
        case (None, a) => a
      }

    val empty: Option[Interval] = None

  def ordered(ints: IndexedSeq[Int]): Boolean =
    foldMapV(ints, orderingMonoid)(i => Some(Interval(true, i, i)))
      .forall(_.ordered)

  enum WC:
    case Stub(chars: String)
    case Part(lStub: String, words: Int, rStub: String)

  lazy val wcMonoid: Monoid[WC] = new:
    def combine(wc1: WC, wc2: WC): WC = (wc1, wc2) match
      case (Stub(c1), Stub(c2)) => Stub(c1 + c2)
      case (Stub(c), Part(l, w, r)) => Part(c + l, w, r)
      case (Part(l, w, r), Stub(c)) => Part(l, w, r + c)
      case (Part(l1, w1, r1), Part(l2, w2, r2)) => Part(l1, w1 + w2 + (if (r1 + l2).isEmpty then 0 else 1), r2)

    val empty: WC = Stub("")

  def count(str: String): Int = {

    def wc(ch: Char): WC =
      if ch.isWhitespace then Part("", 0, "")
      else Stub(ch.toString)

    def unstub(s: String): Int = if s.isEmpty then 0 else 1

    foldMapV(str.toIndexedSeq, wcMonoid)(wc) match
      case Stub(chars) => unstub(chars)
      case Part(left, result, right) => unstub(left) + result + unstub(right)
  }

  given productMonoid[A, B](using ma: Monoid[A], mb: Monoid[B]): Monoid[(A, B)] with
    def combine(x: (A, B), y: (A, B)) = ???
    val empty = ???

  given functionMonoid[A, B](using mb: Monoid[B]): Monoid[A => B] with
    def combine(f: A => B, g: A => B) = ???
    val empty: A => B = a => ???

  given mapMergeMonoid[K, V](using mv: Monoid[V]): Monoid[Map[K, V]] with
    def combine(a: Map[K, V], b: Map[K, V]) = ???
    val empty = ???

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    ???

end Monoid
