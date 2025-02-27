package probprog.impl

import cats.Traverse
import probprog.Domain
import probprog.Language

import scala.collection.parallel.CollectionConverters._

class Parallelize[L <: Language](threads: Int, val language: L) extends Language {
  type F[T] = language.F[T]
  type Dist[T] = language.Dist[T]

  def normal(mean: Double, deviation: Double) = language.normal(mean, deviation)
  def bernoulli(p: Double) = language.bernoulli(p)
  def uniformRange(range: Range) = language.uniformRange(range)
  def uniformContinuous(min: Double, max: Double) = language.uniformContinuous(min, max)

  def flatMapF[T, U](v: F[T])(f: T => F[U]): F[U] = language.flatMapF(v)(f)
  def mapF[T, U](v: F[T])(f: T => U): F[U] = language.mapF(v)(f)

  def sample[T](dist: Dist[T])(implicit domain: Domain[T]): F[T] = 
    language.sample(dist)(domain)

  def observe[T](dist: Dist[T], value: T): F[T] =
    language.observe(dist, value)

  def if_[T](cond: Boolean, ifTrue: => F[T], ifFalse: => F[T]): F[T] =
    language.if_(cond, ifTrue, ifFalse)

  def pure_[T](v: T): F[T] = language.pure_(v)

  def sequence_[T, S[_]](fs: S[F[T]])(implicit t: Traverse[S]): F[S[T]] = language.sequence_(fs)

  def run[T](prg: F[T], n: Long): Result[T] = {
    val perThread: Long = n / threads

    (0 until threads).toList.par.map { _ => 
      language.run(prg, perThread)
    } .reduce(_ ++ _)
  }
}
