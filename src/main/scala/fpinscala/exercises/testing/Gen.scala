package fpinscala.exercises.testing

import fpinscala.exercises.parallelism.*
import fpinscala.exercises.parallelism.Par.Par
import fpinscala.exercises.state.*
import fpinscala.exercises.testing.Gen.*
import fpinscala.exercises.testing.Prop.*

import java.util.concurrent.{ExecutorService, Executors}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

trait Prop:
  self =>

  def check: Boolean

  def &&(other: Prop): Prop = new Prop {
    override def check: Boolean = self.check && other.check
  }

object Prop:
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???

object Gen:
  def unit[A](a: => A): Gen[A] = ???

  extension [A](self: Gen[A])
    def flatMap[B](f: A => Gen[B]): Gen[B] = ???

trait Gen[A]:
  def map[B](f: A => B): Gen[B] = ???
  def flatMap[B](f: A => Gen[B]): Gen[B] = ???

trait SGen[+A]
