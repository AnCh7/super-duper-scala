package week2

import scala.collection.mutable

/*
  Написать калькулятор, используя за основу свои классы I (Int) и D (Double) из предыдущего задания.
  Условие: Операции должны состоять из 2х и более операндов (чисел), например: a + b * c + x / z

  Выполнено хорошо.
  Минусы: Мутабельные коллекции.
  Бросаются exceptions вместо Try / Either / Option.
  Code convention: calculate_rpn.
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

  private def calculateRPN(symbols: List[String]): Option[Double] = {
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
        case _ => None
      }
      else stack.push(symbol.toDouble)
    }
    Some(stack.pop)
  }

  def calculate(expression: String): Either[String, Double] = {
    val symbols = expression.split(' ').toList
    for (s <- symbols) {
      assert(s.nonEmpty)
      assert(isOperator(s) | isNumber(s) | isLeftParenthesis(s) | isRightParenthesis(s))
    }
    calculateRPN(fromInfixToPostfix(symbols)) match {
      case Some(x) => Right(x)
      case None => Left("Not supported operator")
    }
  }
}

object CalculatorApp extends App {

  var expression = "3 + 2"
  var result = Calculator.calculate(expression)
  result.fold(e => println(e), x => assert(x == 5))
  println(expression + s" = $result")

  expression = "5 - 2"
  result = Calculator.calculate(expression)
  result.fold(e => println(e), x => assert(x == 3))
  println(expression + s" = $result")

  expression = "2 * 2"
  result = Calculator.calculate(expression)
  result.fold(e => println(e), x => assert(x == 4))
  println(expression + s" = $result")

  expression = "9 / 3"
  result = Calculator.calculate(expression)
  result.fold(e => println(e), x => assert(x == 3))

  expression = "3 + ( 9 / 3 )"
  result = Calculator.calculate(expression)
  result.fold(e => println(e), x => assert(x == 6))

  expression = "3 + ( 2 * 5 ) + ( 2 / 2 )"
  result = Calculator.calculate(expression)
  result.fold(e => println(e), x => assert(x == 14))
  println(expression + s" = $result")
}