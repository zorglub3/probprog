package probprog

import cats.data.IndexedStateT
import cats.{FlatMap, Functor, Foldable, Applicative, Traverse}

abstract class Language[E[_]: Functor : FlatMap : Applicative] {
  type EvalState
  type F[T] = IndexedStateT[E, EvalState, EvalState, T]
  type Result[T] = Iterable[(T, Double)]

  def normal(mean: Double, deviation: Double): Distribution.Normal =
    new Distribution.Normal(mean, deviation)
  def bernoulli(p: Double): Distribution.Bernoulli =
    new Distribution.Bernoulli(p)
  def uniformRange(range: Range): Distribution.UniformRange =
    new Distribution.UniformRange(range)
  def uniformContinuous(min: Double, max: Double): Distribution.UniformContinuous =
    new Distribution.UniformContinuous(min, max)

  def sample[T](dist: Distribution[T])(implicit domain: Domain[T]): F[T]
  def observe[T](dist: Distribution[T], value: T): F[T]
  def if_[T](cond: Boolean, ifTrue: => F[T], ifFalse: => F[T]): F[T]

  def pure_[T](v: T): F[T] = IndexedStateT.pure[E, EvalState, T](v)
  def sequence_[T](fs: Iterable[F[T]]): F[Unit] = {
    fs.foldLeft(pure_(())) { case (b, a) => {
      b.flatMap(_ => a).flatMap(_ => pure_(()))
    } }
  }

  def run[T](prog: F[T], n: Long): Result[T]

  // Implicits
  // TODO - put them here
}
