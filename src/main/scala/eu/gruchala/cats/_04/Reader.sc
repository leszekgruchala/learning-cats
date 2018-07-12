import cats.data.Reader

case class Cat(name: String, favoriteFood: String)

val catName: Reader[Cat, String] = Reader(cat => cat.name)
catName.run(Cat("Garfield", "lasagne")) //Reads property defined in the Reader

val greetKitty: Reader[Cat, String] = catName.map(name => s"Hello $name")
greetKitty.run(Cat("Heathcliff", "junk food"))

//We can combine Readers of the same input parameter
val feedKitty: Reader[Cat, String] =
  Reader(cat => s"Have a nice bowl of ${cat.favoriteFood}")

val greetAndFeed: Reader[Cat, String] =
  for {
    greet <- greetKitty
    feed  <- feedKitty
  } yield s"$greet. $feed."

greetAndFeed(Cat("Garfield", "lasagne")) // or
greetAndFeed.run(Cat("Garfield", "lasagne"))

//Exercise
case class Db(
  userNames: Map[Int, String],
  passwords: Map[String, String]
)

type DbReader[A] = Reader[Db, A]

def findUsername(userId: Int): DbReader[Option[String]] =
  Reader(_.userNames.get(userId))

def checkPassword(
  username: String,
  password: String): DbReader[Boolean] =
  Reader(_.passwords.get(username).exists(_ == password))

def checkLogin(
  userId: Int,
  password: String): DbReader[Boolean] = {
  import cats.syntax.applicative._
  findUsername(userId).flatMap(_.fold(false.pure[DbReader]) { user =>
    checkPassword(user, password)
  })
}

//Check exercise
val users = Map(
  1 -> "dade",
  2 -> "kate",
  3 -> "margo"
)

val passwords = Map(
  "dade"  -> "zerocool",
  "kate"  -> "acidburn",
  "margo" -> "secret"
)

val db = Db(users, passwords)

checkLogin(1, "zerocool").run(db)
// res10: cats.Id[Boolean] = true

checkLogin(4, "davinci").run(db)
// res11: cats.Id[Boolean] = false
