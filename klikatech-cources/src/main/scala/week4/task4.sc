// 4. Partial functions: Напишите функцию, вычисляющую квадратный корень
// любого неотрицательного числа.

val squareRoot = new PartialFunction[Int, Double] {
  override def isDefinedAt(x: Int): Boolean = x >= 0
  override def apply(v1: Int): Double = math.sqrt(v1)
}

val is1 = squareRoot.isDefinedAt(-1)
val is2 = squareRoot.isDefinedAt(0)
val is3 = squareRoot.isDefinedAt(4)

val a1 = squareRoot.apply(-1)
val a2 = squareRoot.apply(0)
val a3 = squareRoot.apply(4)

val s1 = squareRoot(-1)
val s2 = squareRoot(0)
val s3 = squareRoot(4)

val squareRoot2: PartialFunction[Int, Double] = {
  case n: Int if n >= 0 => math.sqrt(n)
}

val is21 = squareRoot2.isDefinedAt(-1)
val is22 = squareRoot2.isDefinedAt(0)
val is23 = squareRoot2.isDefinedAt(4)

try {
  squareRoot2.apply(-1)
} catch {
  case me: MatchError => println("Matching error" + me.getMessage())
  case e: Exception => println(e.getMessage)
}

val a22 = squareRoot2.apply(0)
val a23 = squareRoot2.apply(4)

try {
  squareRoot2(-1)
} catch {
  case me: MatchError => println("Matching error" + me.getMessage())
  case e: Exception => println(e.getMessage)
}

val s22 = squareRoot2(0)
val s23 = squareRoot2(4)