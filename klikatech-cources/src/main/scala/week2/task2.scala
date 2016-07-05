package week2

import scala.collection.mutable

/*
  Написать калькулятор, используя за основу свои классы I (Int) и D (Double) из предыдущего задания.
  Условие: Операции должны состоять из 2х и более операндов (чисел), например: a + b * c + x / z
*/

object Calculator {

  val addition = "+"
  val subtraction = "-"
  val multiplication = "*"
  val division = "/"

  def isOperator(input: String): Boolean = {
    input match {
      case `addition` | `subtraction` | `multiplication` | `division` => true
      case _ => false
    }
  }

  def isNumber(input: String): Boolean = input.forall(_.isDigit)
  def isLeftParenthesis(input: String): Boolean = input.equals("(")
  def isRightParenthesis(input: String): Boolean = input.equals(")")

  def fromInfixToPostfix(symbols: List[String]): List[String] = {

    val stack = new mutable.Stack[String]
    val queue = new mutable.Queue[String]

    for (symbol <- symbols) {

      if (isNumber(symbol)) queue += symbol

      if (isOperator(symbol)) {
        while (stack.nonEmpty && isOperator(stack.top)) queue += stack.pop
        stack.push(symbol)
      }

      if (isLeftParenthesis(symbol)) stack.push(symbol)

      if (isRightParenthesis(symbol)) {
        while (!isLeftParenthesis(stack.top)) queue += stack.pop
        stack.pop
      }
    }

    while (stack.nonEmpty) {
      if (isLeftParenthesis(stack.top)) sys.error("Parentheses balance error")
      else queue += stack.pop
    }

    queue.toList
  }

  private def calculate_rpn(symbols: List[String]): Double = {

    val stack = new mutable.Stack[Double]

    for (symbol <- symbols) {
      if (isOperator(symbol)) symbol match {
        case `addition` =>
          val x = stack.pop + stack.pop
          stack.push(x)
        case `subtraction` =>
          val x = stack.pop
          stack.push(stack.pop - x)
        case `multiplication` =>
          val x = stack.pop * stack.pop
          stack.push(x)
        case `division` =>
          val x = stack.pop
          stack.push(stack.pop / x)
        case _ => throw new RuntimeException(s"Symbol $symbol is invalid operator")
      }

      else stack.push(symbol.toDouble)
    }

    stack.pop
  }

  def calculate(expression: String): Double = {
    val symbols = expression.split(' ').toList
    for (s <- symbols) {
      assert(s.nonEmpty)
      assert(isOperator(s) | isNumber(s) | isLeftParenthesis(s) | isRightParenthesis(s))
    }
    calculate_rpn(fromInfixToPostfix(symbols))
  }
}

object CalculatorApp extends App {

  var expression = "3 + 2"
  var result = Calculator.calculate(expression)
  assert(result == 5)
  println(expression + s" = $result")

  expression = "5 - 2"
  result = Calculator.calculate(expression)
  assert(result == 3)
  println(expression + s" = $result")

  expression = "2 * 2"
  result = Calculator.calculate(expression)
  assert(result == 4)
  println(expression + s" = $result")

  expression = "9 / 3"
  result = Calculator.calculate(expression)
  assert(result == 3)

  expression = "3 + ( 9 / 3 )"
  result = Calculator.calculate(expression)
  assert(result == 6)

  expression = "3 + ( 2 * 5 ) + ( 2 / 2 )"
  result = Calculator.calculate(expression)
  assert(result == 14)
  println(expression + s" = $result")
}