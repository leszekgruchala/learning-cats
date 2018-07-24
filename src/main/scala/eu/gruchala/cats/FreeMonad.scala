package eu.gruchala.cats

import cats.arrow.FunctionK
import cats.effect.IO

//Thanks to https://www.youtube.com/watch?v=cxMo1RMsD0M

//Free Monad is not tied to cats, but here I have access to ~> and IO
object FreeMonad {

  import scala.language.higherKinds

  import cats.{Monad, ~>}

  sealed trait Free[F[_], A] {
    import Free._

    final def map[B](f: A => B): Free[F, B] = flatMap(a => pure(f(a)))

    final def flatMap[B](f: A => Free[F, B]): Free[F, B] = FlatMap(this, f)

    //nt - natural transformation
    final def foldMap[G[_]: Monad](nt: F ~> G): G[A] = this match {
      case Pure(a) => Monad[G].pure(a)
      case Lift(fa) => nt(fa)
      case FlatMap(inner, f) =>
        Monad[G].flatMap(inner.foldMap(nt))(e => f(e).foldMap(nt))
    }
  }

  object Free {

    def pure[F[_], A](a: A): Free[F, A] = Pure(a)

    def liftM[F[_], A](fa: F[A]): Free[F, A] = Lift(fa)

    final case class Pure[F[_], A](a: A) extends Free[F, A]
    final case class Lift[F[_], A](fa: F[A]) extends Free[F, A]
    //E is a phantom/existential type as we define it and use it but do not return it
    final case class FlatMap[F[_], E, A](self: Free[F, E], f: E => Free[F, A]) extends Free[F, A]
  }

  sealed trait Disk[A]

  object Disk {
    final case class Read(file: String) extends Disk[Array[Byte]]
    //or to ease our lives
    final def read(file: String): Free[Disk, Array[Byte]] = Free.liftM(Read(file))

    final case class Write(file: String, bytes: Array[Byte]) extends Disk[Unit]
    final case class Delete(file: String) extends Disk[Boolean]
  }

  type DiskM[A] = Free[Disk, A]

  import Disk._
  def program: Free[Disk, Boolean] = for {
    contents <- Free.liftM(Read("hi.txt")) //or contents <- read("hi.txt")
    _ <- Free.liftM(Write("hi2.txt", contents ++ "monad".getBytes("UTF-8")))
    _ <- Free.liftM(Write("logs.txt", "free".getBytes("UTF-8")))
    back <- Free.liftM(Delete("evil.txt"))
  } yield back

  val productionCodeNaturalTransformation: FunctionK[Disk, IO] = new FunctionK[Disk, IO] {
    override def apply[A](fa: Disk[A]): IO[A] = fa match {
      case Read(file) => IO {
        println(s"Reading file $file")
        s"contents of file $file".getBytes("UTF-8")
      }
      case Write(file, bytes) => IO {
        println(s"Writing file $file with contents ${new String(bytes, "UTF-8")}")
        ()
      }
      case Delete(file) => IO {
        println(s"Deleting file $file")
        (file.length % 2) == 0
      }
    }
  }

  val testNaturalTransformation: FunctionK[Disk, IO] = new FunctionK[Disk, IO] {
    override def apply[A](fa: Disk[A]): IO[A] = fa match {
      case Read(file) => IO("always success".getBytes("UTF-8"))
      case Write(file, bytes) => IO(())
      case Delete(file) => IO(true)
    }
  }

  //BOOM!
  val result: IO[Boolean] = program.foldMap(productionCodeNaturalTransformation)
  val testResult: IO[Boolean] = program.foldMap(testNaturalTransformation)
}
