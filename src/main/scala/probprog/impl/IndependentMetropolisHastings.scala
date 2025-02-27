package probprog.impl

import cats.Traverse
import cats.data.StateT
import probprog.Distribution
import probprog.Domain
import probprog.Language

import scala.util.Random

class IndependentMetropolisHastings extends Language with StdDistributions {
  type EvalState = MHState
  type F[T] = StateT[Option, EvalState, T]

  def flatMapF[T, U](v: F[T])(f: T => F[U]): F[U] = v.flatMap(f)
  def mapF[T, U](v: F[T])(f: T => U): F[U] = v.map(f)

  case class MHState(
    rngSeed: Long,
    sigma: Double,
    u: Double,
    w: Double
  )

  def getState: F[EvalState] = StateT.get
  def setState(s: EvalState): F[Unit] = StateT.set(s)
  def guard(v: Boolean): F[Unit] = StateT.liftF(Option.when(v)( () ))

  def sample[T](dist: Distribution[T])(implicit domain: Domain[T]): F[T] = {
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
      _ <- guard(state.u < sigma * p / state.w)
      _ <- setState(state.copy(sigma = sigma * p))
    } yield value
  }

  def if_[T](cond: Boolean, ifTrue: => F[T], ifFalse: => F[T]): F[T] = 
    if(cond) { ifTrue } else { ifFalse }

  def pure_[T](v: T): F[T] = StateT.pure(v)

  def sequence_[T, S[_]](fs: S[F[T]])(implicit t: Traverse[S]): F[S[T]] =
    t.sequence(fs)

  def init(u: Double, w: Double): MHState = MHState(Random.nextLong(), 1.0, u, w)

  def run[T](prg: F[T], n: Long): Result[T] = {
    val v = prg.run(init(0.0, 1.0))

    v match {
      case None => Iterable.empty
      case Some(v) => {
        var r = v._2
        var w = v._1.sigma

        val builder = Iterable.newBuilder[T]

        (1L until n).foreach { s =>
          prg.run(init(Random.nextDouble(), w)).foreach { vv =>
            w = vv._1.sigma
            r = vv._2
          }

          builder += r
        }

        builder.result().map { v => (v, 1.0) }
      }
    }
  }
}
