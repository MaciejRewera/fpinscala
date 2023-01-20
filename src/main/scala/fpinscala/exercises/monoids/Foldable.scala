package fpinscala.exercises.monoids

trait Foldable[F[_]]:
  import Monoid.{endoMonoid, dual}

  extension [A](as: F[A])
    def foldRight[B](acc: B)(f: (A, B) => B): B =
      ???

    def foldLeft[B](acc: B)(f: (B, A) => B): B =
      ???

    def foldMap[B](f: A => B)(using mb: Monoid[B]): B =
      as.foldLeft(mb.empty)((b, a) => mb.combine(b, f(a)))

    def combineAll(using ma: Monoid[A]): A =
      as.foldLeft(ma.empty)(ma.combine)

    def toList: List[A] =
      as.foldRight(List.empty[A])(_ :: _)

object Foldable:

  given Foldable[List] with
    extension [A](as: List[A])
      override def foldRight[B](acc: B)(f: (A, B) => B): B = as.foldRight(acc)(f)
      override def foldLeft[B](acc: B)(f: (B, A) => B): B = as.foldLeft(acc)(f)
      override def toList: List[A] = as

  given Foldable[IndexedSeq] with
    extension [A](as: IndexedSeq[A])
      override def foldRight[B](acc: B)(f: (A, B) => B) = as.foldRight(acc)(f)
      override def foldLeft[B](acc: B)(f: (B, A) => B) = as.foldLeft(acc)(f)
      override def foldMap[B](f: A => B)(using mb: Monoid[B]): B = Monoid.foldMapV(as, mb)(f)

  given Foldable[LazyList] with
    extension [A](as: LazyList[A])
      override def foldRight[B](acc: B)(f: (A, B) => B) = as.foldRight(acc)(f)
      override def foldLeft[B](acc: B)(f: (B, A) => B) = as.foldLeft(acc)(f)

  import fpinscala.exercises.datastructures.Tree

  given Foldable[Tree] with
    import Tree.{Leaf, Branch}
    extension [A](as: Tree[A])
      override def foldRight[B](acc: B)(f: (A, B) => B) =
        ???
      override def foldLeft[B](acc: B)(f: (B, A) => B) =
        ???
      override def foldMap[B](f: A => B)(using mb: Monoid[B]): B =
        ???

  given Foldable[Option] with
    extension [A](as: Option[A])
      override def foldRight[B](acc: B)(f: (A, B) => B) =
        ???
      override def foldLeft[B](acc: B)(f: (B, A) => B) =
        ???
      override def foldMap[B](f: A => B)(using mb: Monoid[B]): B =
        ???
