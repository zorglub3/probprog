package probprog

import cats.data.IndexedStateT
import cats.{FlatMap, Functor, Foldable, Applicative, Traverse}

abstract class Language {
  type F[T]

  def flatMapF[T, U](v: F[T])(f: T => F[U]): F[U]
  def mapF[T, U](v: F[T])(f: T => U): F[U]

  type Result[T] = Iterable[(T, Double)]

  implicit class FSyntax[T](v: F[T]) {
    def flatMap[U](f: T => F[U]): F[U] = flatMapF(v)(f)
    def map[U](f: T => U): F[U] = mapF(v)(f)
  }

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

  def pure_[T](v: T): F[T]
  def sequence_[T](fs: Iterable[F[T]]): F[Unit]

  def run[T](prog: F[T], n: Long): Result[T]
}
