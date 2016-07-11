package week3

/*
   Задание:
   Класс для представления точки на плоскости. Точка характеризуется парой координат.
   Реализовать:
   1) toString, equals, hashCode;
   2) перемещение точки на произвольный вектор;
   3) расстояние между точками;
   4) сравнение точек;
   5) в какой координатной четверти лежит точка;
   6) являются ли две точки симметричными относительно начала отсчёта;
   7) являются ли три точки коллинеарными (т.е. лежащими на одной прямой).
*/

object Quadrant extends Enumeration {
  type Quarter = Value
  val First, Second, Third, Fourth, Axis = Value
}

class Point(xc: Int, yc: Int) {

  def this() = this(0, 0)

  private var x: Int = xc
  private var y: Int = yc

  def xCoordinate(): Int = x

  def yCoordinate(): Int = y

  def move(a: Int, b: Int): Unit = {
    x = x + a
    y = y + b
  }

  def set(a: Int, b: Int): Unit = {
    x = a
    y = b
  }

  def distance(that: Point): Double = {
    val a = that.x - this.x
    val b = that.y - this.y
    math.sqrt(a * a + b * b)
  }

  def <(that: Point): Boolean = (this.x <= that.x) && (this.y < that.y)

  def >(that: Point): Boolean = (this.x >= that.x) && (this.y > that.y)

  def quadrant(): Quadrant.Value = {
    if (x > 0 && y > 0) Quadrant.First
    else if (x < 0 && y > 0) Quadrant.Second
    else if (x < 0 && y < 0) Quadrant.Third
    else if (x > 0 && y < 0) Quadrant.Fourth
    else Quadrant.Axis
  }

  def isSymmetrical(that: Point): Boolean = {
    Math.abs(this.x) == Math.abs(that.x) && Math.abs(this.y) == Math.abs(that.y)
  }

  def isCollinear(p2: Point, p3: Point): Boolean = {
    val s = 1 / 2 * ((x * p2.y + p2.x * p3.y + p3.x * y) - (p2.x * y + p3.x * p2.y + x * p3.y))
    s == 0
  }

  def printPoint() {
    println(toString)
  }

  override def toString = s"Point($x, $y)"

  def canEqual(other: Any): Boolean = other.isInstanceOf[Point]

  override def equals(other: Any): Boolean = other match {
    case that: Point =>
      (that canEqual this) &&
        x == that.x &&
        y == that.y
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(x, y)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

object PointApp extends App {

  val p1 = new Point(2, 2)
  val p2 = new Point(3, 4)

  p1.move(3, 3)
  assert(p1.xCoordinate == 5)
  assert(p1.yCoordinate == 5)

  p2.set(2, 3)
  assert(p2.xCoordinate == 2)
  assert(p2.yCoordinate == 3)

  val distance = p1.distance(p2)
  assert(distance == 3.605551275463989)

  assert(new Point(2, 3) > new Point(1, 1))
  assert(new Point(3, 4) < new Point(5, 6))

  assert(new Point(3, 4).quadrant() == Quadrant.First)
  assert(new Point(-3, 4).quadrant() == Quadrant.Second)
  assert(new Point(-3, -4).quadrant() == Quadrant.Third)
  assert(new Point(3, -4).quadrant() == Quadrant.Fourth)
  assert(new Point(0, 4).quadrant() == Quadrant.Axis)

  assert(!p1.isSymmetrical(p2))
  assert(new Point(-2, -2).isSymmetrical(new Point(2, 2)))

  assert(new Point(-3, 3).isCollinear(new Point(0, 3), new Point(3, 3)))

  p1.printPoint()
  println(p1.toString)

  assert(!p1.equals(p2))
  assert(new Point(2, 2).equals(new Point(2, 2)))
}