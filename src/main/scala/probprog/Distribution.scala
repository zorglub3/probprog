package probprog

import scala.util.Random
import scala.math.{exp, sqrt, Pi}

sealed trait Distribution[T] {
  def sample(rnd: Random): T
  def observe(v: T): Double
}

object Distribution {
  class Normal(mean: Double, deviation: Double) extends Distribution[Double] {
    def sample(rnd: Random): Double = rnd.nextGaussian() * deviation + mean
    def observe(v: Double): Double = {
      val x = (v - mean) / deviation
      exp(- x / 2.0) / sqrt(2.0 * Pi)
    }
  }

  class Flip(p: Double) extends Distribution[Boolean] {
    def sample(rnd: Random): Boolean = rnd.nextDouble() < p
    def observe(v: Boolean): Double = if(v) { p } else { 1.0 - p }
  }
}
