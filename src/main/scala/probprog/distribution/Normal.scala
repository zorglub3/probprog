package probprog.distribution

import probprog.Distribution

import scala.math.Pi
import scala.math.exp
import scala.math.sqrt
import scala.util.Random

class Normal(mean: Double, deviation: Double) extends Distribution[Double] {
  require(deviation > 0.0)

  def sample(rnd: Random): Double = rnd.nextGaussian() * deviation + mean
  def observe(v: Double): Double = {
    val x = (v - mean) / deviation
    exp(-x / 2.0) / sqrt(2.0 * Pi)
  }
}
