package week2

import java.util.concurrent.atomic.AtomicLong

/*
В Scala имеется scala.util.Random, для генерации псевдослучайных чисел,
алгоритм https://en.wikipedia.org/wiki/Linear_congruential_generator

Пример использования:
val r = scala.util.Random
val n1 = r.nextInt
n1: Int = -39139337
val n2 = r.nextInt
n2: Int = -1286993272
- видно, что при каждом вызове r.nextInt (где r - иммутабельная переменная),
генерируются различные числа, что является понятием side effect

Задача, написать генератор псевдослучайных чисел (свою реализацию функции nextInt), и убрать сайд-эфект в генерации псевдо-чисел,
т.е. чтобы при повторном вызове r.nextInt - число оставалось идентичным (без сайд-эффектов),
а) если r - иммутабельный, то r.nextInt - не меняет свое поведение;
б) если создается новый инстанс r2 - только в этом случае r2.nextInt будет выдавать отличное от r.nextInt, значение.
*/

class NewRandom() {
  private val prg = new scala.util.Random
  private val rnd = prg.nextInt()
  def nextInt(): Int = rnd
}

object RndApp extends App {
  val r = new scala.util.Random
  println(r.nextInt())
  println(r.nextFloat())
  println(r.nextString(5))

  val newRnd = new NewRandom()
  println(newRnd.nextInt())
  println(newRnd.nextInt())

  val newRnd2 = new NewRandom()
  println(newRnd2.nextInt())
  println(newRnd2.nextInt())
}


class RandomNoSideEffect private(val seed: AtomicLong) {

  private val multiplier = 0x5DEECE66DL
  private val addend = 0xBL
  private val mask = (1L << 48) - 1

  def next(bits: Int = 32): Int = {

    var oldSeed = 0L
    var nextSeed = 0L

    do {
      oldSeed = seed.get()
      nextSeed = (oldSeed * multiplier + addend) & mask
    } while (!seed.compareAndSet(oldSeed, oldSeed))
    (nextSeed >>> (48 - bits)).toInt
  }
}

object RandomNoSideEffect {

  def apply(seed: AtomicLong) = new RandomNoSideEffect(seed)
  def apply() = new RandomNoSideEffect(new AtomicLong(scala.util.Random.nextInt()))

  def main(args: Array[String]): Unit = {
    val rnd = RandomNoSideEffect(new AtomicLong(100))
    Console.println(rnd.next())
    Console.println(rnd.next())

    val rnd2 = RandomNoSideEffect(new AtomicLong(-100))
    Console.println(rnd2.next())
    Console.println(rnd2.next())

    val rnd3 = RandomNoSideEffect()
    Console.println(rnd3.next())
    Console.println(rnd3.next())

    val rnd4 = RandomNoSideEffect()
    Console.println(rnd4.next())
    Console.println(rnd4.next())
  }
}


