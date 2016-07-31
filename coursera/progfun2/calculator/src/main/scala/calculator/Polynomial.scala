package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double], c: Signal[Double]): Signal[Double] = {
    new Signal((b() * b()) - (4 * a() * c()))
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double], c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    val delta = computeDelta(a, b, c)()
    new Signal(delta match {
      case x if x > 0 => Set((-b() - math.sqrt(x)) / (2 * a()), (-b() + math.sqrt(x)) / (2 * a()))
      case x if x == 0 => Set(-b() / (2 * a()))
      case _ => Set.empty
    })
  }
}
