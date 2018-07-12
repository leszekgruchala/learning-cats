import cats.Eq
import cats.instances.int._

val eqInt = Eq[Int]

eqInt.eqv(123, 123)
eqInt.eqv(123, 124)
//eqInt.eqv(123, "123") //does not compile

import cats.syntax.eq._

123 === 1231
123 =!= 124


import cats.instances.option._

//Some(1) === None //does not compile
Option(1) === None

import cats.syntax.option._
1.some === None
1.some =!= None

import java.util.Date
import cats.instances.long._

implicit val dateEqual = Eq.instance[Date] {
  (date1, date2) =>
    date1.getTime === date2.getTime
}

new Date() === new Date()


final case class Cat(name: String, age: Int, color: String)

val cat1 = Cat("Garfield",   35, "orange and black")
val cat2 = Cat("Heathcliff", 30, "orange and black")

val optionCat1 = Option(cat1)
val optionCat2 = Option.empty[Cat]

import cats.instances.string._
implicit val eqCat = Eq.instance[Cat] {
  (cat1, cat2) =>
    cat1.age === cat2.age &&
    cat1.color === cat2.color &&
    cat1.name === cat2.name
}

cat1 === cat1
cat1 === cat2

optionCat1 =!= optionCat2
