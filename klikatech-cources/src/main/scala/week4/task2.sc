// 2. Function literal: Напишите функцию, котороя принимает на вход
// список натуральных чисел и возвращает список, содержащий
// элементы первого списка возведённые в квадрат.
// Использование циклов не допускается.

val power: (List[Int]) => List[Int] = {
  case head :: Nil => List(head * head)
  case head :: tail => (head * head) :: power(tail)
  case Nil => List.empty
  case _ => List.empty
}

val p1 = power(List(1, 2, 3, 4))
val p2 = power(List())
val p3 = power(List.empty)
val p4 = power(Nil)
val p5 = power(null)