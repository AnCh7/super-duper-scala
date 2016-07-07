def factorial(n: Int): Int =
  if (n == 0) 1
  else n * factorial(n - 1)

def factorial_tr(n: Int, accumulator : Int): Int =
  if (n == 0) accumulator
  else factorial_tr(n - 1, accumulator * n)

factorial(10)
factorial_tr(10, 1)
