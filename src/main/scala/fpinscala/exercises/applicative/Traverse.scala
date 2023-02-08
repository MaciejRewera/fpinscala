package fpinscala.exercises.applicative

import fpinscala.answers.monads.Functor
import fpinscala.answers.state.State
import fpinscala.answers.monoids.{Monoid, Foldable}
import Applicative.Const

trait Traverse[F[_]] extends Functor[F], Foldable[F]:
  self =>

  extension [A](fa: F[A])
    def traverse[G[_]: Applicative, B](f: A => G[B]): G[F[B]] =
      fa.map(f).sequence

  extension [G[_]: Applicative, A](fga: F[G[A]])
    def sequence: G[F[A]] =
      fga.traverse(ga => ga)

  type Id[A] = A

  object Id:
    given idMonad: Monad[Id] with
      def unit[A](a: => A) = a
      extension[A] (a: A)
        override def flatMap[B](f: A => B): B = f(a)

  extension [A](fa: F[A])
    def map[B](f: A => B): F[B] =
      fa.traverse[Id, B](f)(using Id.idMonad)

    override def foldMap[B](f: A => B)(using mb: Monoid[B]): B =
      fa.traverse[Const[B, _], Nothing](f)

    override def foldLeft[B](acc: B)(f: (B, A) => B): B =
      ???

    override def toList: List[A] =
      fa.mapAccum(List.empty[A])((a, s) => ((), a :: s))._2.reverse

    def mapAccum[S, B](s: S)(f: (A, S) => (B, S)): (F[B], S) =
      fa.traverse(a => 
        for
          s1 <- State.get[S]
          (b, s2) = f(a, s1)
          _  <- State.set(s2)
        yield b
      ).run(s)

    def zipWithIndex2: F[(A, Int)] =
      fa.traverse((a: A) => for {
        i <- State.get[Int]
        _ <- State.set(i + 1)
      } yield (a, i)).run(0)._1

    def zipWithIndex: F[(A, Int)] =
      fa.mapAccum(0)((a, s) => ((a, s), s + 1))(0)

    def reverse: F[A] =
      fa.mapAccum(fa.toList.reverse)((_, s) => (s.head, s.tail))._1

    def fuse[M[_], N[_], B](f: A => M[B], g: A => N[B])(using m: Applicative[M], n: Applicative[N]): (M[F[B]], N[F[B]]) =
      ???

  def compose[G[_]: Traverse]: Traverse[[x] =>> F[G[x]]] = new:
    extension [A](fa: F[G[A]])
      override def traverse[H[_]: Applicative, B](f: A => H[B]): H[F[G[B]]] =
        ???

case class Tree[+A](head: A, tail: List[Tree[A]])

object Traverse:
  given listTraverse: Traverse[List] with
    extension [A](as: List[A])
      override def traverse[G[_]: Applicative, B](f: A => G[B]): G[List[B]] =
        val g = summon[Applicative[G]]
        as.foldRight(g.unit(List.empty[B]))((a, acc) => f(a).map2(acc)(_ :: _))

  given optionTraverse: Traverse[Option] with
    extension [A](oa: Option[A])
      override def traverse[G[_]: Applicative, B](f: A => G[B]): G[Option[B]] =
        oa match
          case Some(a) => f(a).map(Some(_))
          case None => summon[Applicative[G]].unit(None)

  given treeTraverse: Traverse[Tree] = new:
    extension [A](ta: Tree[A])
      override def traverse[G[_]: Applicative, B](f: A => G[B]): G[Tree[B]] =
        val newHead: G[B] = f(ta.head)
        val newTail: G[List[Tree[B]]] = ta.tail.traverse(t => t.traverse(f))
        
        newHead.map2(newTail)((h, t) => Tree(h, t))
  
  given mapTraverse[K]: Traverse[Map[K, _]] with
    extension [A](m: Map[K, A])
      override def traverse[G[_]: Applicative, B](f: A => G[B]): G[Map[K, B]] =
        ???
