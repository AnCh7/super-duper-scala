package week3

/*
  Реализовать конструкцию whileLoop
*/

object task1 {
  def main(args: Array[String]) {
    var x = 1
    while (x < 100) {
      println("Value of x: " + x)
      x += 1
    }
  }
}

// Выполнено неверно

object task1_1 {

  def whileLoop(condition: => Boolean)(operation: => Unit): Unit = {
    if (condition) {
      operation
      whileLoop(condition)(operation)
    }
  }

  def main(args: Array[String]) {

    val message = "Scala runs on the JVM, so Java and Scala stacks can be freely mixed for totally seamless integration.".toCharArray

    var i = 0
    whileLoop(i <= message.length - 1) {
      println(message(i).equals('a'))
      i += 1
    }
  }
}

