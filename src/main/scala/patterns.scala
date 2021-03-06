package lambdaconf.patterns

import matryoshka._
import monocle._
import scalaz._, Scalaz._, concurrent.Task

object exercise1 {

  /** we have some potential errors*/
  sealed trait Error
  case object NotInTheMoodToTalkToHumans extends Error
  case object MiningBitcoinNoTimeToTalk extends Error

  /** this will give you String (but potentially an error as well) */
  val iWillGiveYouString: Double => Error \/ String =
    (d: Double) =>
      if (d > 10) d.toString.right else NotInTheMoodToTalkToHumans.left

  /** this will give you Int (but potentially an error as well) */
  val iWillGiveYouInt: Long => Error \/ Int =
    (l: Long) => if (l < 0) l.toInt.right else MiningBitcoinNoTimeToTalk.left

  // NB Use monadic for-comprehensions to short-circuit on error
  // def calculation(d: Double, l: Long): Error \/ (String, Int) =
  //   for {
  //     s <- iWillGiveYouString(d)
  //     i <- iWillGiveYouInt(l)
  //   } yield (s, i)

  // NB Use applicative builder (TIE fighter operator) with Validation to accumulate errors
  def calculation(d: Double, l: Long): ValidationNel[Error, (String, Int)] =
    (iWillGiveYouString(d).validationNel |@|
      iWillGiveYouInt(l).validationNel) { (s, i) =>
      (s, i)
    }

  /** If I'm not interested with an error - I want to ignore it */
  val result: Option[(String, Int)] = calculation(20.0, -3).toOption

  /** What is a potential issue with the returned type Error \/ (String, Int) */
  /** Run all those function calls and reason about the result, what can we do about it? */
  // def main(args: Array[String]): Unit = {
  //   println(calculation(20.0, -2))
  //   println(calculation(20.0, 6))
  //   println(calculation(5.0, -2))
  //   println(calculation(5.0, 6))
  // }

}

object exercise2 {

  case class User(login: String)

  sealed trait Item
  case object Apple extends Item
  case object Orange extends Item

  /** Basket contains items */
  // NB you could make this more general and parameterize Basket on A; and later require that A
  // forms a semigroup
  case class Basket(items: List[Item])

  /** I have to baskets */
  val basket1 = Basket(items = List(Apple, Orange, Apple, Apple))
  val basket2 = Basket(items = List(Apple, Apple, Orange))

  implicit def BasketSemigroup: Semigroup[Basket] = new Semigroup[Basket] {
    def append(b1: Basket, b2: => Basket): Basket = Basket(b1.items ++ b2.items)
  }

  /** Could I merge two baskets together */
  val finalBasket: Basket = basket1 |+| basket2

  val user1 = User("john")
  val user2 = User("pawel")
  val user3 = User("jeff")

  /** Could I merge two sessions together */
  val session1: Map[User, Basket] = Map(user1 -> basket1, user2 -> basket2)
  val session2: Map[User, Basket] =
    Map(user1 -> finalBasket, user2 -> basket1, user3 -> basket2)

  // NB this is why using semigroups is powerful!
  val session: Map[User, Basket] = session1 |+| session2

  /** Implement toBasket method, what needs to be added for this to work? */
  def toBasket(maybeItem: Option[Item]): Basket = ???
}

object exercise3 {

  /** What is the space of possible solutions of this method */
  def sum(i1: Int, i2: Int): Int = ???

  /** Write abstract version of method sum. What is the space of possible solutions of its body? */
  // def sum(???): ??? = ???
}

object exercise4 {

  /** What is the space of possible solutions of this method */
  def sum(list: List[Int]): Int =
    list.fold(0)(_ + _)

  /** Write the most abstract version of method sum. What is the space of possible solutions of its body? */
  // def sum(???): ??? = ???
}

object exercise5 {

  /** problem 1 */
  val problem1: Option[Option[String]] = Some(Some("hello"))
  val solution1: Option[String] = ???

  /** problem 2 */
  val problem2: List[Task[Int]] = List(
    Task.now(1),
    Task.now(3)
  )
  val solution2: Task[List[Int]] = ???

  /** problem 3 */
  case class User(id: Int)
  val ids = List(1, 2, 3)
  val fetch: Int => Task[User] = (id: Int) =>
    Task.delay {
      User(id)
  }

  val solution: Task[List[User]] = ???

}

object exercise6 {

  val plus: (Int, Int) => Int = _ + _

  def plusOperation[F[_]: Apply](
      f1: F[Int],
      f2: F[Int]
  ): F[Int] = (f1 |@| f2)(plus)

  /** second argument not closed over F, what now? */
  object problem1 {
    def plusOperation[F[_]](
        f1: F[Int],
        i: Int
    ): F[Int] = ???
  }

  /** we get new parameter seed F[String], last paramter is now String => F[Int]... can you make it work? */
  object problem2 {
    def plusOperation[F[_]](
        seed: F[String],
        f1: F[Int],
        f2: String => F[Int]
    ): F[Int] = ???
  }
}
