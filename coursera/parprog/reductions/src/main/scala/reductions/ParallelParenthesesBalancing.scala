package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer (new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  val openBracket = '('
  val closeBracket = ')'

  def isOpen(char: Char) = char.equals(openBracket)
  def isClose(char: Char) = char.equals(closeBracket)

  def balance(chars: Array[Char]): Boolean = {
    @tailrec
    def balanceCount(i: Int, counter: Int): Int = {
      if (counter == -1) -1
      else if (i == chars.length) counter
      else if (isOpen(chars(i))) balanceCount(i + 1, counter + 1)
      else if (isClose(chars(i))) balanceCount(i + 1, counter - 1)
      else balanceCount(i + 1, counter)
    }
    balanceCount(0, 0) == 0
  }

  /**
    * Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int): (Int, Int) = {
      var mNest = 0
      var totalNest = 0
      for (current <- idx until until) {
        if (isOpen(chars(current))) totalNest += 1
        if (isClose(chars(current))) totalNest -= 1
        if (totalNest < mNest) {
          mNest = totalNest
        }
      }
      (mNest, totalNest)
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from <= threshold) {
        traverse(from, until, 0, 0)
      }
      else {
        val middle = from + (until - from) / 2
        val (l, r) = parallel(reduce(from, middle), reduce(middle, until))
        (Math.min(l._1, l._2 + r._1), l._2 + r._2)
      }
    }
    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!
}
