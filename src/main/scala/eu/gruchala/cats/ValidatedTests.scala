package eu.gruchala.cats

import cats.syntax.validated._
import cats.syntax.apply._

case class Result(a: String, b: String)
object ValidatedTests {

  ("asd".validNel[String] -> "asd".validNel[String]).mapN(Result)
}
