package probprog.util

object Normalize {
  def apply[T](m: Iterable[(T, Double)]): Iterable[(T, Double)] = {
    val total = m.map(_._2).sum
    m.map(pair => (pair._1, pair._2 / total))
  }
}

