package lambdaconf.types

import matryoshka._
import monocle._
import scalaz._

import Scalaz._

/**
  * Create ADT representing geometric figures using traits and case classes
  * A figure can be a rectangle or trapezoid or cicrle
  * Rectangle has side a and side b
  * Trapezoid has base a, base b and height h
  * Circle has radius r
  *
  * Can you think of other representation using different machinery?
  */
object exercise1 {
  sealed trait Figure
  case class Rectangle(a: Double, b: Double)
  case class Trapezoid(a: Double, b: Double, h: Double)
  case class Circle(r: Double)

  // Could also represent this as a series of Eithers
}

/**
  * Call me maybe
  */
object exercise2 {

  /** Maybe is of kind *. Change it to be * -> * so that it can hold values of any type */
  sealed trait Maybe[A]
  case class Just[A](value: A)
  case object Empty

  val gotIt = Just("hello")
  val nah = Empty
  val gotNum = Just(10)
  case class User(name: String)
  val maybeUser = Just(User("Swift"))
}

/**
  * Call each function with any argument so that the code compiles.
  * Please provide the types explicitly in [ ]
  */
object exercise3 {
  def func1[A](a: A): A = a
  def func2[F[_], A](f: F[A]): F[A] = f
  def func3[E[_, _], A, B](e: E[A, B]): E[A, B] = e
  def func4[T[_[_]], F[_]](t: T[F]): T[F] = t

  func1[Int](2)
  func2[Option, Int](Some(1))
  func3[Map, Int, String](Map(1 -> "Hi"))

  class Container[F[_]]()
  func4[Container, List](new Container[List])
}

object exercise4 {
  sealed trait Example[F[_]] {
    def value: F[String]
  }

  new Example[Either[?, Int]] { // <-- ???
    def value: Either[String, Int] = Right(2) // returns a set of types
  }
}

/** Explore the mysteries of magic box */
object exercise5 {

  /** This is a magic box. */
  trait MagicBox[A] {
    // Here, B is an existential type; we are hiding it in the top-level type params of MagicBox
    type B
    def create[C](c: C): MagicBox[C]
    def get: A

    def map(f: A => B): MagicBox[B] = create[B](f(get))
  }

  /** create a class IntMagicBox[A] that is a MagicBox where B is an Int */
  class IntMagicBox[A](a: A) extends MagicBox[A] {
    // We can fix B here, and access it elsewhere using path-dependent typing
    type B = Int
    def create[C](c: C): MagicBox[C] = new IntMagicBox(c)
    def get: A = a
  }

  /**
    Method transformAndExtract should take a MagicBox and function f, apply to map method and then
    retrive the value using get method.

    Implement transformAndExtract
    */
  def transformAndExtract[A](mb: MagicBox[A])(f: A => mb.B): mb.B =
    mb.map(f).get

  val strIntMagicBox = new IntMagicBox[String]("hello existential")

  val length: String => Int = _.length

  /** call transformAndExtract with instance of intMagicBox and function length to test that it works*/
  transformAndExtract[String](strIntMagicBox)(length)

}
