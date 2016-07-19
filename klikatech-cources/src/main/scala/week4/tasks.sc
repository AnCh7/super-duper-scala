// 1. Higher order functions: Напишите прокси-генератор:
// функцию, которая принимает на вход две функции: f и g и возвращает функцию,
// которая применяет g к результату выполнения f и возвращает результат выполнения f.

def convertToInt(s: String): Int = s.toInt
def nonNegative(i: Int): Boolean = i >= 0
def nonNegativeString(s: String, f: String => Int, g: Int => Boolean): Boolean = {
  g(f(s))
}

val i1 = convertToInt("1")
val i2 = convertToInt("-1")

val b1 = nonNegative(i1)
val b2 = nonNegative(i2)

nonNegativeString("1", convertToInt, nonNegative)
nonNegativeString("-1", convertToInt, nonNegative)


// 2. Function literal: Напишите функцию, котороя принимает на вход
// список натуральных чисел и возвращает список, содержащий
// элементы первого списка возведённые в квадрат. Использование циклов не допускается.

def power(numbers: List[Int]): List[Int] = List.empty
List(1, 2, 3, 4)




// 3. Partially applied functions: Используя прокси-генератор из первого задания
// напишите функцию-обёртку, выводящую в консоль результаты выполнения любой функции.


// 4. Partial functions: Напишите функцию,
// вычисляющую квадратный корень любого неотрицательного числа.

