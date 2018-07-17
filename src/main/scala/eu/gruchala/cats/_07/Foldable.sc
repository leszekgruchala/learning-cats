
List(1, 2, 3).foldLeft(List.empty[Int])((acc, elem) => elem :: acc)
List(1, 2, 3).foldRight(List.empty[Int])((elem, acc) => elem :: acc)

val list = List(1, 2, 3, 4)

def map[A, B](l: List[A])(f: A => B): List[B] =
  l.foldRight(List.empty[B])((elem, acc) => f(elem) :: acc)
map(list)(_ + 1)

def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
  l.foldRight(List.empty[B])((elem, acc) => f(elem) ::: acc)
map(list)(_ + 1)

def filter[A](l: List[A])(cond: A => Boolean): List[A] =
  l.foldRight(List.empty[A])((elem, acc) => if (cond(elem)) elem :: acc else acc)
filter(list)(_ > 2)

import cats.Monoid
import cats.instances.int._
def sum[A](l: List[A])(implicit A: Monoid[A]): A =
  l.foldRight(A.empty)((elem, acc) => A.combine(elem, acc))
sum(list)

import cats.Foldable
import cats.instances.list._

Foldable[List].foldLeft(list, 0)(_ + _)

//foldRight is done in terms of Eval monad, thus it's stack safe
import cats.Eval
import cats.instances.stream._
val bigData = (1 to 100000).toStream
val eval = Foldable[Stream].foldRight(bigData, Eval.now(0L)) { (acc, eval) =>
  eval.map(_ + acc)
}

eval.value

import cats.instances.option._
Foldable[Option].nonEmpty(Option(42))

//with Monoid support
Foldable[List].combineAll(List(1, 2, 3))
import cats.instances.string._
Foldable[List].foldMap(list)(_.toString)


val ints = List(Vector(1, 2, 3), Vector(4, 5, 6))
import cats.instances.vector._
(Foldable[List] compose Foldable[Vector]).combineAll(ints)

import cats.syntax.foldable._
List(1, 2, 3).combineAll
List(1, 2, 3).foldMap(_.toString)
