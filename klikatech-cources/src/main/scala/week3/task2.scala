package week3

import scala.annotation.tailrec
import scala.util.Random

/*
  Реализовать конструкцию run { body } until { condition }
  Пример: val value = run { Random.nextInt(10) } until (_ == 0)

  Update - Выполнено хорошо. Минусы: Отсутствие @tailrec.
*/

class run[A](statement: => A) {

  @tailrec
  final def until(condition: A => Boolean): A = {
    val result = statement
    if (condition(result)) result
    else until(condition)
  }

}

object run {
  def apply[A](body: => A) = new run(body)
}

object Task2App extends App {

  val value = run {
    val x = Random.nextInt(1000007860)
    println(x)
    x
  } until (_ == 0)

  print("Done")
}