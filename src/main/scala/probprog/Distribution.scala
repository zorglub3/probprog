package probprog

sealed trait Distribution[T]

object Distribution {
  class Normal(expected: Double, variance: Double) extends Distribution[Double]
  class Flip(p: Double) extends Distribution[Boolean]
}
