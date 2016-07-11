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
