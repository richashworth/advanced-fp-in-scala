package lambdaconf.typeclasses

import matryoshka._
import monocle._

/**
  * Implement Eq type class
  */
object exercise1 {

  /** Before we start, try to reason what is wrong with the code below? */
  def check(i: Int): String =
    if (i == "1") "It is one!" else "It is something else then 1"
  check(1)
  check(2)

  /**
    * 1. Create Eq type class with method equalz checking for equality of two object of type A
    * 2. Create extension method ===
    * 3. Create instance for Int
    * 4. Reimplelemnt check method using Eq
    */
  trait Eq[A] {
    def equalz(a1: A, a2: A): Boolean
  }

  object Eq {

    def apply[A: Eq]: Eq[A] = implicitly[Eq[A]]
    implicit def IntEq: Eq[Int] = new Eq[Int] {
      def equalz(i1: Int, i2: Int): Boolean = i1 == i2
    }

    object Ops {
      implicit class Syntax[A: Eq](a1: A) {
        def ===(a2: A): Boolean = Eq[A].equals(a1, a2)
      }
    }

  }

  import Eq.Ops._

  def foo[A: Eq](a1: A, a2: A): String =
    if (a1 === a2) "hurray" else "boooo..."

}

// This was the worked example in the lecture
object type_classes {
  sealed trait Ordering
  case object EQ extends Ordering
  case object LT extends Ordering
  case object GT extends Ordering

  // Ord is a type class; we're not using a trait in an OO way here
  trait Ord[A] {
    def compare(l: A, r: A): Ordering
  }

  object Ord {
    def apply[A](implicit a: Ord[A]): Ord[A] = a

    implicit val OrdInt: Ord[Int] = new Ord[Int] {
      def compare(l: Int, r: Int): Ordering =
        if (l < r) LT
        else if (l > r) GT
        else EQ
    }

    implicit val OrdString: Ord[String] = new Ord[String] {
      def compare(l: String, r: String): Ordering =
        if (l < r) LT
        else if (l > r) GT
        else EQ
    }
  }

  // Context bounds mean the below signatures are equivalent.
  // def sort[A: Ord](v: List[A])(implicit o: Ord[A]): List[A] = ???
  def sort[A: Ord](v: List[A]): List[A] = {
    ???
  }

  implicit class OrdSyntax[A: Ord](l: A) {
    def <(r: A): Boolean = Ord[A].compare(l, r) == LT
    def >(r: A): Boolean = Ord[A].compare(l, r) == GT
    def ===(r: A): Boolean = Ord[A].compare(l, r) == EQ
  }

  sort(9 :: 5 :: 3 :: 11 :: 4 :: Nil)
  sort("foo" :: "bar" :: Nil)

}
