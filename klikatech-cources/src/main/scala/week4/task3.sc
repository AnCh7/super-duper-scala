// 3. Partially applied functions: Используя прокси-генератор из первого задания
// напишите функцию-обёртку, выводящую в консоль результаты выполнения любой функции.

def convertToInt(s: String): Int = s.toInt
def isNegative(i: Int): Boolean = i < 0
def isNegativeString(s: String, f: String => Int, g: Int => Boolean): Boolean = g(f(s))

def printResult(function: => Any): Any = {
  val result = function
  Console.println("Function result - " + "[" + result + "]")
  function
}

printResult(isNegativeString("1", convertToInt, isNegative))


// Переделать (см. п1).

def wrapper[A, B](f: A => B, g: B => Unit): A => Unit = x => g(f(x))
val printSqrtResult = wrapper((x: Double) => math.sqrt(x), (z: Double) => println(s"result = $z"))

printSqrtResult(9)