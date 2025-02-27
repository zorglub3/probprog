package probprog.distribution

import probprog.Distribution

import scala.util.Random

class Bernoulli(p: Double) extends Distribution[Double] {
  require(p >= 0.0)
  require(p <= 1.0)

  def sample(rnd: Random): Double =
    if(rnd.nextDouble() < p) { 1.0 } else { 0.0 }
  def observe(v: Double): Double = {
    if(v.abs < scala.Double.MinPositiveValue) {
      1.0 - p
    } else if((v - 1.0).abs < scala.Double.MinPositiveValue) {
      p
    } else {
      0.0
    }
  }
}
