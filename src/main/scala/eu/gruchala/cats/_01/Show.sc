import cats.Show
import cats.instances.all._

val showInt = Show.apply[Int]
showInt.show(123)

val showString = Show.apply[String]
showString.show("ASD")

import cats.syntax.show._

123.show
"asd".show

import java.util.Date
implicit val dateShow: Show[Date] = Show.show(date => s"${date.getTime}ms since the epoch")

new Date().show



final case class Cat(name: String, age: Int, color: String)
implicit val catShow = Show.show[Cat] { cat =>
  val name  = cat.name.show
  val age   = cat.age.show
  val color = cat.color.show
  s"$name is a $age year-old $color cat."
}
Cat("Garfield", 35, "ginger and black").show
