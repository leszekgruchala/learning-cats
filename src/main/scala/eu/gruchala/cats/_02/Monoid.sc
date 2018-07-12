import cats.Monoid
import cats.Semigroup
import cats.instances.string._


//Semigroup - combine
//Monoid extends Semigroup - empty

Monoid[String].combine("Hi", " there")
Monoid.apply[String].combine("Hi", " there")

Semigroup[String].combine("Hi", " there")

Monoid[String].empty

import cats.instances.int._
import cats.instances.option._

val a = Option(22)
val b = Some(20)
Semigroup[Option[Int]].combine(a, b)

//The syntax
import cats.syntax.semigroup._

a |+| b
1 |+| 2 |+| Monoid[Int].empty

final case class Order(totalCost: Double, quantity: Int)

def add[A : Semigroup](items: List[A]): A = {
  items.reduceLeft(_ |+| _)
}

add(List(1, 2, 3, 4))

val orderA = Order(12.2, 2)
val orderB = Order(1.2, 4)
implicit val orderSemigroup: Semigroup[Order] = new Semigroup[Order] {
  override def combine(x: Order, y: Order): Order =
    Order(x.totalCost + x.totalCost, y.quantity + y.quantity)
}
add(List(orderA, orderB))

import cats.syntax.option._
add(List(1.some, Option(2), Some(3), None, none, none[Int]))
