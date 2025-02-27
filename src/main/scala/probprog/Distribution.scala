package probprog

import scala.math.Pi
import scala.math.exp
import scala.math.sqrt
import scala.util.Random

sealed trait Distribution[T] {
  def sample(rnd: Random): T 
  def observe(v: T): Double

  def sampleN(n: Int, rnd: Random): Seq[T] = {
    val builder = Seq.newBuilder[T]

    for(_ <- 0 until n) { builder += sample(rnd) }

    builder.result()
  }
}

object Distribution {
  class Normal(mean: Double, deviation: Double) extends Distribution[Double] {
    def sample(rnd: Random): Double = rnd.nextGaussian() * deviation + mean
    def observe(v: Double): Double = {
      val x = (v - mean) / deviation
      exp(- x / 2.0) / sqrt(2.0 * Pi)
    }
  }

  class Bernoulli(p: Double) extends Distribution[Double] {
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

  class UniformRange(range: Range) extends Distribution[Int] {
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

  class UniformContinuous(min: Double, max: Double) extends Distribution[Double] {
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
      
  class WeightedSequence[T](values: Seq[T], weights: Seq[Double]) extends Distribution[T] {
    val weightTotal = weights.sum

    def sample(rnd: Random): T = {
      val x = Random.nextDouble()
      var acc = 0.0

      for(i <- 0 until values.length) {
        acc += weights(i) / weightTotal
        if(x < acc) {
          return values(i)
        }
      }

      return values(values.length - 1)
    }

    def observe(v: T): Double = {
      for(i <- 0 until values.length) {
        if(v == values(i)) {
          return weights(i) / weightTotal
        }
      }

      return 0.0
    }
  }
}
