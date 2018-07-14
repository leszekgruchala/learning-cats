import cats.Semigroupal
import cats.instances.option._

Semigroupal[Option].product(Some(1), Some(2))
Semigroupal[Option].product(Some(1), None)

Semigroupal.tuple3(Option(1), Option(1), Option(1))
Semigroupal.tuple3(Option(1), Option(1), Option.empty[Int])

Semigroupal.map3(Option(1), Option(1), Option(1))(_ + _ + _)
Semigroupal.map3(Option(1), Option(1), Option.empty[Int])(_ + _ + _)

//Apply syntax
import cats.syntax.apply._

(Option(123), Option("abc")).tupled


case class Cat(name: String, born: Int, color: String)

(
  Option("Garfield"),
  Option(1978),
  Option("Orange & Black")
).mapN(Cat.apply)

import cats.Monad

import scala.language.higherKinds

import cats.syntax.flatMap._
import cats.syntax.functor._
def product[M[_]: Monad, A, B](x: M[A], y: M[B]): M[(A, B)] = {
//  x.flatMap(a => y.map(b => a -> b))
  for {
    v1 <- x
    v2 <- y
  } yield v1 -> v2
}

//Semigroupal of Either cannot accumulate errors as Either is a Monad and that would break the laws
import cats.data.Validated
import cats.instances.either._ //for Monoid, get Semigroupal implementation of Either

type ErrorOr[A] = Either[List[String], A]
Semigroupal[ErrorOr].product(
  Left(List("Error 1")),
  Left(List("Error 2"))
)
//Left(List(Error 1))

//With Validated it's possible to accumulate as it has instance of Semigroupal but not Monoid
import cats.instances.list._ //provides combine (Semigroup) to the List, so Validated knows how to combine errors
type AllErrorsOr[A] = Validated[List[String], A]
Semigroupal[AllErrorsOr].product(
  Validated.invalid(List("Error 1")),
  Validated.invalid(List("Error 2"))
)
//Invalid(List(Error 1, Error 2))

import cats.syntax.applicative._ //for .pure
import cats.syntax.applicativeError._ //for .raiseError

123.pure[AllErrorsOr]
List("Badness").raiseError[AllErrorsOr, Int]

case class User(name: String, age: Int)

import cats.syntax.either._ //for .ensure
def getValue(k: String, m: Map[String, String]): ErrorOr[String] = m.get(k).toRight(List(s"$k field is missing"))
def parseInt(s: String): ErrorOr[Int] = {
  Either.catchOnly[NumberFormatException](s.toInt).leftMap(_ => List(s"$s is not a number"))
}
def nonBlank(s: String): ErrorOr[String] = Right(s).ensure(List(s"$s cannot be blank"))(_.nonEmpty)
def nonNegative(i: Int): ErrorOr[Int] = Right(i).ensure(List(s"$i must be non-negative"))(_ >= 0)

def readName(m: Map[String, String]): ErrorOr[String] = {
  getValue("name", m).flatMap(nonBlank)
}

def readAge(m: Map[String, String]): ErrorOr[Int] =
  getValue("age", m)
    .flatMap(nonBlank)
    .flatMap(parseInt)
    .flatMap(nonNegative)

val data = Map("name" -> "Franek", "age" -> "39")

val user: AllErrorsOr[User] = (
  readName(data).toValidated,
  readAge(data).toValidated
).mapN(User.apply)
