import cats.Eval

def factorial(n: BigInt): Eval[BigInt] =
  if (n == 1) Eval.now(n)
  else Eval.defer(factorial(n -1).map(_ * n))

factorial(50000).value


def foldRight[A, B](as: List[A], acc: Eval[B])(fn: (A, Eval[B]) => Eval[B]): Eval[B] = as match {
  case head :: tail =>
    fn(head, Eval.defer(foldRight(tail, acc)(fn)))

  case Nil => acc
}

foldRight(List.fill(50000)(1), Eval.now(0)){
  case (elem, acc) => acc.map(_ + elem)
}.value
