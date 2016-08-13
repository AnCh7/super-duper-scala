import scala.annotation.tailrec

/*
  4) Реализовать функцию вычисляющую факториал.
  Обеспечить устойчивость к StackOverflowError и переполнению типа.
  Hint: recursion, pattern matching.

  Update - Верно (можно заменить x.toInt == 0 на pattern matching by value).
*/

def factorial(n: Int) = {
  val zero = BigInt(0)
  @tailrec
  def factorialInner(n: BigInt, accumulator: BigInt): BigInt = n match {
    case `zero` => accumulator
    case _ => factorialInner(n - 1, accumulator * n)
  }
  factorialInner(n, 1)
}

val result = factorial(100000)