import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global

Future(List.empty[Int])

import cats.instances.future._
import cats.syntax.applicative._ //for .pure

List.empty[Int].pure[Future]


import scala.language.higherKinds
import cats.Applicative
import cats.syntax.apply._ //for .mapN
def listTraverse[F[_]: Applicative, A, B](list: List[A])(f: A => F[B]): F[List[B]] =
  list.foldLeft(List.empty[B].pure[F]) { (acc, item) =>
    (acc, f(item)).mapN(_ :+ _)
  }

def listSequence[F[_]: Applicative, B](list: List[F[B]]): F[List[B]] =
  listTraverse(list)(identity)

import cats.instances.vector._

listSequence(List(Vector(1, 2), Vector(3, 4))) //Vector(List(1, 3), List(1, 4), List(2, 3), List(2, 4)) :D Semigroupal!

import cats.instances.option._

def process(inputs: List[Int]): Option[List[Int]] =
  listTraverse(inputs)(n => if (n % 2 == 0) Some(n) else None)

process(List(2, 4, 6))
process(List(1, 2, 3))//None because of Vector Semigroupal (Monad.flatMap)

import cats.data.Validated
import cats.instances.list._

type ErrorsOr[A] = Validated[List[String], A]

def processV(inputs: List[Int]): ErrorsOr[List[Int]] =
  listTraverse(inputs) { n =>
    if (n % 2 == 0) Validated.valid(n)
    else Validated.invalid(List(s"$n is not even"))
  }

processV(List(2, 4, 6))
processV(List(1, 2, 3)) //Invalid(List(1 is not even, 3 is not even)) because of Validated semigroupal

import cats.Traverse
import scala.concurrent.duration._

val hostnames = List(
  "alpha.example.com",
  "beta.example.com",
  "gamma.demo.com"
)

def getUptime(hostname: String): Future[Int] =
  Future(hostname.length * 60) // just for demonstration

val totalUptime: Future[List[Int]] =
  Traverse[List].traverse(hostnames)(getUptime)
Await.result(totalUptime, 1.second)

val numbers = List(Future(1), Future(2), Future(3))

val numbers2: Future[List[Int]] =
  Traverse[List].sequence(numbers)

Await.result(numbers2, 1.second)

import cats.syntax.traverse._

Await.result(hostnames.traverse(getUptime), 1.second)
Await.result(numbers.sequence, 1.second)
