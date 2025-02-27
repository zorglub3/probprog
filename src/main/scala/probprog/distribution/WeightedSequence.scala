package probprog.distribution

import probprog.Distribution

import scala.util.Random

class WeightedSequence[T](seq: Seq[(T, Double)]) extends Distribution[T] {
  require(seq.length > 0)
  require(seq.forall(_._2 > 0.0))

  val weightTotal = seq.map(_._2).sum

  def sample(rnd: Random): T = {
    val x = Random.nextDouble() * weightTotal
    var acc = 0.0

    for(p <- seq) {
      acc += p._2

      if(x < acc) {
        return p._1
      }
    }

    return seq(seq.length - 1)._1
  }

  def observe(v: T): Double = {
    for(p <- seq) {
      if(v == p._1) {
        return p._2 / weightTotal
      } 
    }

    return 0.0
  }
}
