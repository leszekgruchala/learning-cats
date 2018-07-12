import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.language.implicitConversions

import cats.data.EitherT

//type Response[A] = Future[Either[String, A]]
type Response[A] = EitherT[Future, String, A]

val powerLevels = Map(
  "Jazz"      -> 6,
  "Bumblebee" -> 8,
  "Hot Rod"   -> 10
)

import cats.syntax.either._
def getPowerLevel(autobot: String): Response[Int] = EitherT(Future {
  powerLevels.get(autobot).fold(s"$autobot unreachable".asLeft[Int])(_.asRight)
})

import cats.instances.future._
def getPowerLevel2(autobot: String): Response[Int] = {
  powerLevels.get(autobot).fold(EitherT.left[Int](Future(s"$autobot unreachable")))(i => EitherT.right(Future(i)))
}

def read[A](r: Response[A]) = Await.result(r.value, 10.seconds)

read(getPowerLevel("Jazz"))
read(getPowerLevel("A"))

read(getPowerLevel2("Jazz"))
read(getPowerLevel2("A"))

import cats.syntax.applicative._

123.pure

def canSpecialMove(ally1: String, ally2: String): Response[Boolean] = {

  val combinedPower = for {
    p1 <- getPowerLevel(ally1)
    p2 <- getPowerLevel(ally2)
  } yield p1 + p2

//  combinedPower.flatMap(power => EitherT.right(Future.successful(power > 15)))
  combinedPower.map(_ > 15)
}

read(canSpecialMove("Jazz", "Hot Rod"))
read(canSpecialMove("Jazz", "Bumblebee"))
read(canSpecialMove("Jazz", "A"))

def tacticalReport(ally1: String, ally2: String): String = {
  val f = canSpecialMove(ally1, ally2).fold(s => s, can =>
    if (can) s"$ally1 and $ally2 are ready to roll out!" else s"$ally1 and $ally2 need a recharge")

  Await.result(f, 10.seconds)
}


tacticalReport("Jazz", "Bumblebee")
// res28: String = Jazz and Bumblebee need a recharge.

tacticalReport("Bumblebee", "Hot Rod")
// res29: String = Bumblebee and Hot Rod are ready to roll out!

tacticalReport("Jazz", "Ironhide")
// res30: String = Comms error: Ironhide unreachable”

