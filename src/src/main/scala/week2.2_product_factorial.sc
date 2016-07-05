def product(f: Int => Int)(a: Int, b: Int): Int = {
  if (a > b) 1
  else f(a) * product(f)(a + 1, b)
}

def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int = {
  if (a > b) zero
  else combine(f(a), mapReduce(f, combine, zero)(a + 1, b))
}

def newProduct(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, (x, y) => x * y, 1)(a, b)

product(x => x * x)(3, 4)
newProduct(x => x * x)(3, 4)

def fact(n: Int) = {
  product(x => x)(1, n)
}

fact(5)




