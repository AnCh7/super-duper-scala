// 1. Higher order functions: Напишите прокси-генератор:
// функцию, которая принимает на вход две функции: f и g и возвращает функцию,
// которая применяет g к результату выполнения f и возвращает результат выполнения f.

def convertToInt(s: String): Int = s.toInt
def isNegative(i: Int): Boolean = i < 0
def isNegativeString(s: String, f: String => Int, g: Int => Boolean): Boolean = g(f(s))

val i1 = convertToInt("1")
val i2 = convertToInt("-1")
val b1 = isNegative(i1)
val b2 = isNegative(i2)

val r1 = isNegativeString("1", convertToInt, isNegative)
assert(!r1)

val r2 = isNegativeString("-1", convertToInt, isNegative)
assert(r2)

// Нет полиморфной имплементации, функция принимает лишний параметр. Нужно переделать.

def wrapper[A, B](f: A => B, g: B => B): A => B = x => g(f(x))
val stringIncrement = wrapper((x: String) => x.toInt, (y: Int) => y + 1)

stringIncrement("10")

// Получилась композиция функций, ожидается другое поведение:
// функцию, которая применяет g к результату выполнения f и возвращает результат выполнения f

def newWrapper[A, B](f: A => B, g: B => Unit) = (x: A) => {
  val r = f(x)
  g(r)
  r
}

val stringGreaterThanZero = newWrapper((x: String) => x.toInt, (x: Int) => assert(x > 0))
stringGreaterThanZero("10")
stringGreaterThanZero("-100")
