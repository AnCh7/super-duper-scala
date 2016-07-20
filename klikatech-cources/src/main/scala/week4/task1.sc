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