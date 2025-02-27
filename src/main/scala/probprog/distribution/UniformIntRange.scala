package probprog.distribution

import probprog.Distribution

import scala.util.Random

class UniformIntRange(range: Range) extends Distribution[Int] {
  val size = range.length.toDouble

  def sample(rnd: Random): Int = range(rnd.nextInt(range.length))

  def observe(v: Int): Double = {
    if(range.contains(v)) {
      1.0 / size
    } else {
      0.0
    }
  }
}
