package probprog

import cats.data.IndexedStateT
import cats.{FlatMap, Functor, Foldable}

abstract class Language[E[_]: Functor : FlatMap : Foldable] {
  type EvalState
  type F[T] = IndexedStateT[E, EvalState, EvalState, T]

  def normal(mean: Double, deviation: Double): Distribution.Normal =
    new Distribution.Normal(mean, deviation)
  def flip(p: Double): Distribution.Flip =
    new Distribution.Flip(p)
  def uniformRange(range: Range): Distribution.UniformRange =
    new Distribution.UniformRange(range)
  def uniformContinuous(min: Double, max: Double): Distribution.UniformContinuous =
    new Distribution.UniformContinuous(min, max)

  def sample(dist: Distribution): F[Double]
  def observe(dist: Distribution, value: Double): F[Double]
  def if_[T](cond: Boolean, ifTrue: => F[T], ifFalse: => F[T]): F[T]
  def sequence_[T](fs: Iterable[F[T]]): F[Unit]

  def run[T](prog: F[T], n: Long): Result[T]
}

trait Result[T] {
  def prob(v: T): Double
  def histogram(): Seq[(T, Double)]
}
