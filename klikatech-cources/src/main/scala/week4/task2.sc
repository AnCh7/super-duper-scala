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

// Можно было использовать withFilter + map

def newPower2(list: List[Int]) = {
  if (list != null) list.withFilter(x => x > 0).map(x => x * x)
  else List.empty
}

// или for comprehension,

def newPower2_2(list: List[Int]) =
  if (list == null) List.empty
  else
    for {
      x <- list if x > 0
    } yield x * x

// как вариант ещё Partial Function + collect.

val newPower2_3 = new PartialFunction[Int, Int] {
  def apply(x: Int) = x * x
  def isDefinedAt(x: Int) = x > 0
}

List(1, 2, 3, 4).collect(newPower2_3)
List(-1, 2, -33, 4, 0, 100).collect(newPower2_3)
List().collect(newPower2_3)
List.empty.collect(newPower2_3)
Nil.collect(newPower2_3)

val newPower2_4: PartialFunction[Int, Int] = {
  case x: Int if x > 0 => x * x
}

List(1, 2, 3, 4).collect(newPower2_4)
List(-1, 2, -33, 4, 0, 100).collect(newPower2_4)
List().collect(newPower2_4)
List.empty.collect(newPower2_4)
Nil.collect(newPower2_4)
