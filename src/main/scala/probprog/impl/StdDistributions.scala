package probprog.impl

import probprog.Distribution
import probprog.Language
import probprog.distribution._

trait StdDistributions { self: Language =>
  type Dist[T] = Distribution[T]

  def normal(mean: Double, deviation: Double) =
    new Normal(mean, deviation)

  def bernoulli(p: Double) =
    new Bernoulli(p)

  def uniformRange(range: Range) =
    new UniformIntRange(range)

  def uniform(min: Double, max: Double) =
    new Uniform(min, max)

  def uniformSequence[T](seq: Seq[T]) =
    new UniformSequence(seq)

  def weightedSequence[T](seq: Seq[(T, Double)]) =
    new WeightedSequence(seq)
}

