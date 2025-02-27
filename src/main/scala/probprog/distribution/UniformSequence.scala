package probprog.distribution

import probprog.Distribution

import scala.util.Random

class UniformSequence[T](values: Seq[T]) extends Distribution[T] {
  val size = values.length
  val sizeDouble = size.toDouble

  def sample(rnd: Random): T = values(rnd.nextInt(size))

  def observe(v: T): Double = {
    if(values.contains(v)) {
      1.0 / sizeDouble
    } else {
      0.0
    }
  }
}
