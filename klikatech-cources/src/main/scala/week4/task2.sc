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

// Update: Нет проверок на натуральность чисел.
// Обычная рекурсия вместо хвостой.
// foldLeft / map / for comnprehension / etc.

import scala.annotation.tailrec

def power2(list: => List[Int]) = {
  @tailrec
  def process(list: => List[Int], acc: List[Int]): List[Int] = list match {
    case head :: Nil if head > 0 => acc ::: List(head * head)
    case head :: tail if head > 0 => process(tail, acc ::: List(head * head))
    case head :: tail if head <= 0 => process(tail, acc ::: List(0))
    case Nil => List.empty
    case _ => List.empty
  }
  process(list, Nil)
}

val t1 = power2(List(1, 2, 3, 4))
val t2 = power2(List(-1, 2, -33, 4, 0, 100))
val t3 = power2(List())
val t4 = power2(List.empty)
val t5 = power2(Nil)
val t6 = power2(null)