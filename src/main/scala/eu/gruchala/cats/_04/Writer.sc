import cats.Id
import cats.data.{Writer, WriterT}


//Writer - Appended (thus preserved) log with computed result
//Useful for logging operations in multi-threaded environments
type Logged[A] = Writer[Vector[String], A]

//only result
import cats.syntax.applicative._ //.pure
import cats.instances.vector._   //.Writer for Vector
123.pure[Logged] //WriterT

//only log
import cats.syntax.writer._
Vector("log1", "log2").tell //Writer

//log and result
Writer(Vector("message 1", "message 2"), 123) //WriterT
123.writer(Vector("message 1", "message 2")) //Writer

123.writer(Vector("message 1", "message 2")).value //result
123.writer(Vector("message 1", "message 2")).written //log
123.writer(Vector("message 1", "message 2")).run //(log, result)

//logs are preserved with .map and .flatMap
val writer1 = for {
  a <- 10.pure[Logged]
  _ <- Vector("a", "b", "c").tell
  b <- 32.writer(Vector("x", "y", "z"))
} yield a + b

writer1.run //(Vector(a, b, c, x, y, z), 42)

//transform just log
val writer2 = writer1.mapWritten(log => log.map(_.toUpperCase))
writer2.run

//transform both
val writer3 = writer1.bimap(
  log => log.map(_.toUpperCase),
  result => result * 10
)
writer3.run
//or
val writer4 = writer1.mapBoth { case (log, result) =>
  (log.map(_ + "!"), result * 10)
}
writer4.run

//reset - clear the log
writer1.reset.run

//swap log with result
writer1.swap.run //(42,Vector(a, b, c, x, y, z))

1.pure[Logged]

//Exercise
def slowly[A](body: => A): A =
  try body finally Thread.sleep(100)

def factorial(n: Int): Writer[Vector[String], Int] = {
  val ans: WriterT[Id, Vector[String], Int] = slowly {
    if (n == 0) 1.pure[Logged] //type alias needed
    else factorial(n - 1).map(_ * n)
  }
  ans.mapWritten(_ :+ s"fact $n ${ans.value}")
}

import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

Await.result(
  Future.sequence(
    Vector(
      Future(factorial(3).run),
      Future(factorial(3).run)
    )
  ), 5.seconds
)
