package probprog.impl

import cats.data.StateT
import cats.{Eval, FlatMap, Functor}
import scala.util.Random
import probprog.{Language, Distribution, Domain}

class LikelihoodWeight extends Language { 
  type EvalState = LWState
  type F[T] = StateT[Eval, EvalState, T]
  type Dist[T] = Distribution[T]

  def normal(mean: Double, deviation: Double) =
    new Distribution.Normal(mean, deviation)
  def bernoulli(p: Double) =
    new Distribution.Bernoulli(p)
  def uniformRange(range: Range) =
    new Distribution.UniformRange(range)
  def uniformContinuous(min: Double, max: Double) =
    new Distribution.UniformContinuous(min, max)

  def flatMapF[T, U](v: F[T])(f: T => F[U]): F[U] = v.flatMap(f)
  def mapF[T, U](v: F[T])(f: T => U): F[U] = v.map(f)

  case class LWState(rngSeed: Long, sigma: Double)

  def getState: F[EvalState] = StateT.get
  def setState(v: EvalState): F[Unit] = StateT.set(v)
  def init(): EvalState = LWState(Random.nextLong(), 1.0)

  def sample[T](dist: Dist[T])(implicit domain: Domain[T]): F[T] = {
    for {
      state <- getState
      seed = state.rngSeed
      rng = new Random(seed)
      nextSeed = rng.nextLong()
      result = dist.sample(rng)
      _ <- setState(state.copy(rngSeed = nextSeed))
    } yield result
  }

  def observe[T](dist: Dist[T], value: T): F[T] = {
    for {
      state <- getState
      sigma = state.sigma
      p = dist.observe(value)
      _ <- setState(state.copy(sigma = sigma * p))
    } yield value
  }

  def if_[T](cond: Boolean, ifTrue: => F[T], ifFalse: => F[T]): F[T] = 
    if(cond) { ifTrue } else { ifFalse }

  def pure_[T](v: T): F[T] = StateT.pure(v)

  def sequence_[T](fs: Iterable[F[T]]): F[Unit] = {
    fs.foldLeft(pure_(())) { case (b, a) => {
      b.flatMap(_ => a).flatMap(_ => pure_(()))
    } }
  }

  def run[T](prg: F[T], n: Long): Result[T] = {
    (for(_ <- 0L until n) yield prg.run(init()).value)
      .groupBy(_._2)
      .map { case (k, v) => (k, v.map { case (x, _) => x.sigma } .sum) }
  }
}
