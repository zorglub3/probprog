package probprog.util

trait HistogramBin[T] {
  def contains(t: T): Boolean
}

object HistogramBin {
  class OrderedBin[T](min: T, max: T)(implicit ord: Ordering[T]) extends HistogramBin[T] {
    def contains(t: T): Boolean =
      ord.lteq(min, t) && ord.gt(max, t)
  }

  case class DoubleBin(min: Double, max: Double) extends OrderedBin[Double](min, max)
  case class IntBin(min: Int, max: Int) extends OrderedBin[Int](min, max)

  class NominalBin[T](v: T) extends HistogramBin[T] {
    def contains(t: T): Boolean = v == t
  }

  class Tuple2Bin[T, U](bin1: HistogramBin[T], bin2: HistogramBin[U]) extends HistogramBin[(T, U)] {
    def contains(p: (T, U)): Boolean = bin1.contains(p._1) && bin2.contains(p._2)
  }

  class Tuple3Bin[T, U, V](
    bin1: HistogramBin[T], 
    bin2: HistogramBin[U],
    bin3: HistogramBin[V]
  ) extends HistogramBin[(T, U, V)] {
    def contains(p: (T, U, V)): Boolean = 
      bin1.contains(p._1) && bin2.contains(p._2) && bin3.contains(p._3)
  }

  class Tuple4Bin[T, U, V, W](
    bin1: HistogramBin[T], 
    bin2: HistogramBin[U],
    bin3: HistogramBin[V],
    bin4: HistogramBin[W]
  ) extends HistogramBin[(T, U, V, W)] {
    def contains(p: (T, U, V, W)): Boolean = 
      bin1.contains(p._1) && bin2.contains(p._2) && bin3.contains(p._3) && bin4.contains(p._4)
  }

  class VectorBin[T](bins: Vector[HistogramBin[T]]) extends HistogramBin[Vector[T]] {
    def contains(v: Vector[T]): Boolean = 
      bins.zip(v).forall { case (bin, value) => bin.contains(value) }
  }
}
