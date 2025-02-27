package probprog.distribution

import probprog.Distribution

import scala.util.Random

class Uniform(min: Double, max: Double) extends Distribution[Double] {
  require(min < max)

  val size = max - min

  def sample(rnd: Random): Double = rnd.nextDouble() * size + min

  def observe(v: Double): Double = {
    if(v >= min && v < max) {
      1.0 / size
    } else {
      0.0
    }
  }
}
