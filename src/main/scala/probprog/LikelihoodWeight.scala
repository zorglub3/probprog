package probprog

import cats.data.IndexedStateT
import cats.{Eval, FlatMap, Functor}
import scala.util.Random

class LikelihoodWeight extends Language[Eval] {
  type EvalState = LWState

  case class LWState(rngSeed: Long, sigma: Double)

  def getState: F[EvalState] = IndexedStateT.get
  def setState(v: EvalState): F[Unit] = IndexedStateT.set(v)
  def init(): EvalState = LWState(Random.nextLong(), 1.0)

  def sample[T](dist: Distribution[T]): F[T] = {
    for {
      state <- getState
      seed = state.rngSeed
      rng = new Random(seed)
      nextSeed = rng.nextLong()
      result = dist.sample(rng)
      _ <- setState(state.copy(rngSeed = nextSeed))
    } yield result
  }

  def observe[T](dist: Distribution[T], value: T): F[T] = {
    for {
      state <- getState
      sigma = state.sigma
      p = dist.observe(value)
      _ <- setState(state.copy(sigma = sigma * p))
    } yield value
  }

  def if_[T](cond: Boolean, ifTrue: => F[T], ifFalse: => F[T]): F[T] = 
    if(cond) { ifTrue } else { ifFalse }

  def run[T](prg: F[T], n: Long): Result[T] = {
    LWResult(
      (for(_ <- 0L until n) yield prg.run(init()).value)
        .groupBy(_._2)
        .map { case (k, v) => (k, v.map { case (x, _) => x.sigma } .sum) })
  }
}

case class LWResult[T](m: Map[T, Double]) extends Result[T] {
  val total = m.map { case (_, v) => v } .sum

  def prob(v: T): Double = m.getOrElse(v, 0.0) / total

  def histogram(): Seq[(T, Double)] =
    m.toSeq.map { case (k, v) => (k, v / total) }
}
