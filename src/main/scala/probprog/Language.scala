package probprog

import cats.data.IndexedStateT
import cats.{FlatMap, Functor}

abstract class Language[E[_]: Functor : FlatMap] {
  type EvalState
  type F[T] = IndexedStateT[E, EvalState, EvalState, T]

  def normal(mean: Double, deviation: Double): Distribution.Normal =
    new Distribution.Normal(mean, deviation)
  def flip(p: Double): Distribution.Flip =
    new Distribution.Flip(p)

  def sample[T](dist: Distribution[T]): F[T]
  def observe[T](dist: Distribution[T], value: T): F[T]
  def if_[T](cond: Boolean, ifTrue: => F[T], ifFalse: => F[T]): F[T]

  def run[T](prog: F[T], n: Long): Result[T]
}

trait Result[T] {
  def prob(v: T): Double
  def histogram(): Seq[(T, Double)]
}
