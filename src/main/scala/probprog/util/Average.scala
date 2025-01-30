package probprog.util

class Average[T](implicit m: Fractional[T]) {
  def weighted(l: Iterable[(T, Double)]): Double = {
    val weightTotal = l.map { _._2 } .sum

    l.map { p => m.toDouble(p._1) * p._2 / weightTotal } .sum
  }

  def apply(l: Iterable[T]): T = {
    m.div(l.sum, m.fromInt(l.size))
  }
}

