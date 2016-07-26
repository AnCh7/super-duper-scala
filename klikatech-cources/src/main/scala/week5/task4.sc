import scala.annotation.tailrec

/*
  4) Реализовать функцию вычисляющую факториал.
  Обеспечить устойчивость к StackOverflowError и переполнению типа.
  Hint: recursion, pattern matching.
*/

def factorial(n: Int) = {
  @tailrec
  def factorialInner(n: BigInt, accumulator: BigInt): BigInt = n match {
    case x if x.toInt == 0  => accumulator
    case _ => factorialInner(n - 1, accumulator * n)
  }
  factorialInner(n, 1)
}

val result = factorial(100000)