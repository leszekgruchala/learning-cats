import scala.language.higherKinds

trait MyMonad[F[_]] {
  def pure[A](a: A): F[A]

  def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]

  def map[A, B](value: F[A])(f: A => B): F[B] =
    flatMap(value)(a => pure(f(a)))
}

import cats._
import cats.implicits._

val opt1 = Monad[Option].pure(3)

val opt2 = Monad[Option].flatMap(opt1)(a => Some(a + 2))

//using syntax
1.pure[Option]
1.pure[List]

def sumSquare[M[_]: Monad](a: M[Int], b: M[Int]): M[Int] =
  a.flatMap(x => b.map(y => x*x + y*y))

sumSquare(List(1, 2, 3), 1 :: 3 :: Nil)

sumSquare(1: Id[Int], 3: Id[Int])
