package probprog

import cats.data.IndexedStateT
import cats.{FlatMap, Functor}
import scala.util.Random

class MetropolisHastings extends Language[Option] {
  type EvalState = MHState

  case class MHState(
    rngSeed: Long,
    sigma: Double,
    u: Double,
    w: Double
  )

  def getState: F[EvalState] = IndexedStateT.get
  def setState(s: EvalState): F[Unit] = IndexedStateT.set(s)
  def guard(v: Boolean): F[Unit] = IndexedStateT.liftF(Option.when(v)( () ))

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
      _ <- guard(state.u < sigma * p / state.w)
      _ <- setState(state.copy(sigma = sigma * p))
    } yield value
  }

  def if_[T](cond: Boolean, ifTrue: => F[T], ifFalse: => F[T]): F[T] = 
    if(cond) { ifTrue } else { ifFalse }

  def init(u: Double, w: Double): MHState = MHState(Random.nextLong(), 1.0, u, w)

  def run[T](prg: F[T], n: Long): Result[T] = {
    val v = prg.run(init(0.0, 1.0))

    v match {
      case None => MHResult(Vector.empty)
      case Some(v) => {
        var r = v._2
        var w = v._1.sigma

        val builder = Vector.newBuilder[T]

        (1L until n).foreach { s =>
          prg.run(init(Random.nextDouble(), w)).foreach { vv =>
            w = vv._1.sigma
            r = vv._2
          }

          builder += r
        }

        MHResult(builder.result())
      }
    }
  }
}

case class MHResult[T](v: Vector[T]) extends Result[T] {
  val l = v.size.toDouble
  val h = v.groupBy(identity).map { case (k, v) => (k, v.size.toDouble / l) } .toMap

  def prob(v: T): Double = h.getOrElse(v, 0.0)
  def histogram(): Seq[(T, Double)] = h.toSeq
}
