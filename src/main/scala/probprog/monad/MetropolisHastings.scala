package probprog.monad

import cats.data.State
import scala.util.Random
import scala.math.log
import probprog._

class MetropolisHastings extends Language {
  type EvalState = MHState

  case class MHState(rngSeed: Long, sigma: Double)

  def getState = State.get[MHState]
  def setState(s: MHState) = State.set(s)

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

  def if_[T](cond: Boolean, ifTrue: => F[T], ifFalse: => F[T]): F[T] = {
    if(cond) {
      ifTrue
    } else {
      ifFalse
    }
  }

  def init(): MHState = MHState(Random.nextLong(), 1.0)

  // FIXME does not work!
  def run[T](prog: F[T], n: Long): Result[T] = {
    val v = prog.run(init()).value
    var r = v._2
    var w = v._1.sigma

    val builder = Vector.newBuilder[T]

    (1L until n).foreach { s =>
      val vv = prog.run(init()).value
      val rr = vv._2
      val ww = vv._1.sigma
      val alpha = ww / w
      val u = Random.nextDouble()

      if(u < alpha) {
        r = rr
        w = ww
      }

      builder += r
    }

    MHResult(builder.result())
  }
}

case class MHResult[T](v: Vector[T]) extends Result[T] {
  val l = v.size.toDouble
  val h = v.groupBy(identity).map { case (k, v) => (k, v.size.toDouble / l) } .toMap

  def prob(v: T): Double = h.getOrElse(v, 0.0)
  def histogram(): Seq[(T, Double)] = h.toSeq
}
