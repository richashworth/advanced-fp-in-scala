package lambdaconf.effects

import matryoshka._
import monocle._
import scalaz._

import Scalaz._

object exercise1 {

  def putStrLn(line: String): IO[Unit] = IO(() => println(line))

  // NB Note how everything unsafe is wrapped in a thunk in the IO
  final case class IO[A](unsafePerformIO: () => A) { self =>

    final def flatMap[B](f: A => IO[B]): IO[B] = {
      IO(() => f(self.unsafePerformIO()).unsafePerformIO())
    }

    final def map[B](f: A => B): IO[B] =
      IO(unsafePerformIO = () => {
        f(self.unsafePerformIO())
      })
  }

  // object IO {
  //   def apply[A](a: => A): IO[A] = IO(() => a)
  //   def point[A](a: => A): IO[A] = IO(a)
  // }

  implicit val IOMonad: Monad[IO] = ???
}

object exercise2 {
  import exercise1._

  sealed trait ConsoleF[A]

  val program: Free[ConsoleF, Unit] = ???
}

object exercise3 {
  final case class State[S, A]( /* ??? */ ) {
    def evalState(s: S): A = ???
  }

  implicit def StateMonad[S]: Monad[State[S, ?]] = ???
}

object exercise4 {
  import exercise3._

  def sort[A: Order](list: List[A]): List[A] =
    (??? : State[List[A], List[A]]).evalState(list)
}
