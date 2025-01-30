package probprog.util

class Median[T](implicit m: Fractional[T], o: Ordering[T]) {
  def apply(l: Seq[T]): Option[T] = {
    l.size match {
      case 0 => None
      case i if i % 2 == 1 => Some(l.sorted.apply(l.size / 2))
      case i => {
        val sorted = l.sorted
        Some(m.div(m.plus(sorted(l.size / 2), sorted(l.size / 2 - 1)), m.fromInt(2)))
      }
    }
  }
}
