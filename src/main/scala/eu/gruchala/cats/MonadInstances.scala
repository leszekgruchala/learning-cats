package eu.gruchala.cats

import scala.language.reflectiveCalls

import cats.Monad

trait MonadInstances {
  //type lambda, produce a new (anonymous) type with one hole but equivalent to Either[L, β] where L has been fixed
  implicit def eitherMonad[L]:Monad[({ type λ[β] = Either[L, β] })#λ] = new Monad[({ type λ[β] = Either[L, β] })#λ] {
    override def flatMap[A, B](ma: Either[L, A])(f: (A) => Either[L, B]): Either[L, B] = ma.right.flatMap(f)

    override def map[A, B](ma: Either[L, A])(f: (A) => B): Either[L, B] = ma.right.map(f)

    override def tailRecM[A, B](a: A)(f: A => Either[L, Either[A, B]]): Either[L, B] = ???

    override def pure[A](x: A): Either[L, A] = Right(x)
  }
}
