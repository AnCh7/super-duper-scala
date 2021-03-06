/*
  Напишите функцию minmax(values: Array[Int]): Tuple2[Int, Int] возвращающую пару,
  содержащую наименьшее и наибольшее значение.
*/

def minmax(values: Array[Int]): (Int, Int) = {
  if (values.nonEmpty) {
    values.map(i => (i, i)).reduceLeft((x, y) => (math.min(x._1, y._1), math.max(x._2, y._2)))
  }
  else (0, 0)
}

var mm = minmax(Array.emptyIntArray)
mm = minmax(Array(1, 2, 4, 5, 6, 7, 8, 9, 10))
mm = minmax(Array(-1000, 4, Int.MinValue, 556, 0, 4, 458, Int.MaxValue))