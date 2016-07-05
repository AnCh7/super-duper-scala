package week2

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
