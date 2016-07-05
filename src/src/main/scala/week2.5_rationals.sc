class Rational(x: Int, y: Int) {

  require(y != 0, "denominator must be nonzero")

  def this(x: Int) = this(x, 1)

  def numerator = x

  def denominator = y

  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  def <(that: Rational) = numerator * that.denominator < that.numerator * denominator

  def max(that: Rational) = if (this.<(that)) that else this

  def +(that: Rational) = new Rational(
    numerator * that.denominator + that.numerator * denominator,
    denominator * that.denominator)

  def unary_- : Rational = new Rational(-numerator, denominator)

  def -(that: Rational) = this + -that

  override def toString = {
    val g = gcd(numerator, denominator)
    numerator / g + "/" + denominator / g
  }
}

val x = new Rational(1, 2)
x.denominator
x.numerator

val y = new Rational(2, 3)
x + y

val z = new Rational(5, 3)
x - y - z

x < y
x max y

new Rational(2)