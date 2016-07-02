package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || r == 0) 1
    else if (c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    if (chars.isEmpty) false
    else {
      val openBracket = '('
      val closeBracket = ')'
      def isOpen(char: Char) = char.equals(openBracket)
      def isClose(char: Char) = char.equals(closeBracket)

      def isBalanced(chars: List[Char], stack: List[Char]): Boolean = {
        if (chars.isEmpty) stack.isEmpty
        else if (isOpen(chars.head)) {
          isBalanced(chars.tail, chars.head :: stack)
        }
        else if (isClose(chars.head)) {
          if (stack.contains(openBracket))
            isBalanced(chars.tail, stack.tail)
          else
            false
        }
        else isBalanced(chars.tail, stack)
      }

      isBalanced(chars, List())
    }
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money < 0 || coins.isEmpty) 0
    else countChange(money, coins.tail) + countChange(money - coins.head, coins)
  }
}

