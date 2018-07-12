import cats.Functor
import cats.instances.function._
import cats.syntax.functor._

val func1 = (a: Int) => a + 1

val func2 = (a: Int) => a * 2

val func3 = func1.map(func2)

func3(123)

sealed trait Tree[+A]
final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
final case class Leaf[A](value: A) extends Tree[A]

implicit def treeFunctor = new Functor[Tree] {
  override def map[A, B](fa: Tree[A])(f: (A) => B): Tree[B] = {
      fa match {
        case Branch(left, right) =>
          Branch(map(left)(f), map(right)(f))
        case Leaf(value) =>
          Leaf(f(value))
      }
  }
}
def set(a: A): Box[A] = new Box[A]
