package probprog

import cats.data.IndexedStateT
import cats.{FlatMap, Functor, Foldable, Applicative, Traverse}

trait Language {
  type F[T]
  type Dist[T]

  def flatMapF[T, U](v: F[T])(f: T => F[U]): F[U]
  def mapF[T, U](v: F[T])(f: T => U): F[U]

  type Result[T] = Iterable[(T, Double)]

  implicit class FSyntax[T](v: F[T]) {
    def flatMap[U](f: T => F[U]): F[U] = flatMapF(v)(f)
    def map[U](f: T => U): F[U] = mapF(v)(f)
  }

  def normal(mean: Double, deviation: Double): Dist[Double]
  def bernoulli(p: Double): Dist[Double]
  def uniformRange(range: Range): Dist[Int]
  def uniformContinuous(min: Double, max: Double): Dist[Double]

  def sample[T](dist: Dist[T])(implicit domain: Domain[T]): F[T]
  def observe[T](dist: Dist[T], value: T): F[T]
  def if_[T](cond: Boolean, ifTrue: => F[T], ifFalse: => F[T]): F[T]

  def pure_[T](v: T): F[T]

  def sequence_[T, S[_]](fs: S[F[T]])(implicit t: Traverse[S]): F[S[T]]

  def run[T](prog: F[T], n: Long): Result[T]
}
