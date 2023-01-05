package fpinscala.exercises.monoids

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

  def optionMonoid[A]: Monoid[Option[A]] = ???

  def dual[A](m: Monoid[A]): Monoid[A] = new:
    def combine(x: A, y: A): A = m.combine(y, x)
    val empty = m.empty

  def endoMonoid[A]: Monoid[A => A] = ???

  import fpinscala.exercises.testing.{Gen, Prop}
  // import Gen.`**`

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = ???

  def combineAll[A](as: List[A], m: Monoid[A]): A =
    ???

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    ???

  def foldRight[A, B](as: List[A])(acc: B)(f: (A, B) => B): B =
    ???

  def foldLeft[A, B](as: List[A])(acc: B)(f: (B, A) => B): B =
    ???

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    ???

  def par[A](m: Monoid[A]): Monoid[Par[A]] = 
    ???

  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = 
    ???

  def ordered(ints: IndexedSeq[Int]): Boolean =
    ???

  enum WC:
    case Stub(chars: String)
    case Part(lStub: String, words: Int, rStub: String)

  lazy val wcMonoid: Monoid[WC] = ???

  def count(s: String): Int = ???

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
