package probprog

import scala.util.Random
import scala.math.{exp, sqrt, Pi}

sealed trait Distribution {
  def sample(rnd: Random): Double 
  def observe(v: Double): Double
}

object Distribution {
  class Normal(mean: Double, deviation: Double) extends Distribution {
    def sample(rnd: Random): Double = rnd.nextGaussian() * deviation + mean
    def observe(v: Double): Double = {
      val x = (v - mean) / deviation
      exp(- x / 2.0) / sqrt(2.0 * Pi)
    }
  }

  class Flip(p: Double) extends Distribution {
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

  class UniformRange(range: Range) extends Distribution {
    val size = range.length.toDouble
    def sample(rnd: Random): Double = range(rnd.nextInt(range.length)).toDouble
    def observe(v: Double): Double = {
      val vv = v.toInt

      if((v - vv.toDouble).abs > scala.Double.MinPositiveValue) {
        0.0
      } else if(range.contains(vv)) { 
        1.0 / size 
      } else { 
        0.0 
      }
    }
  }

  class UniformContinuous(min: Double, max: Double) extends Distribution {
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
}
