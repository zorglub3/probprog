package probprog.impl

import cats.Traverse
import cats.data.StateT
import probprog.Domain
import probprog.Language
import probprog.Value

import scala.util.Random

class BufferedMetropolisHastings extends Language with StdDistributions { 
  type EvalState = BMHState 
  type F[T] = StateT[Option, EvalState, T]

  def flatMapF[T, U](v: F[T])(f: T => F[U]): F[U] = v.flatMap(f)
  def mapF[T, U](v: F[T])(f: T => U): F[U] = v.map(f)

  case class BMHState(
    prefix: String, 
    counter: Long,
    samples: Map[String, Value],
    rngSeed: Long,
    sigma: Double,
    u: Double, 
    w: Double
  ) {
    def incCounter(): BMHState = copy(counter = counter + 1)
    def addPrefix(p: String): BMHState = copy(prefix = p ++ prefix)
    def popPrefix(): BMHState = copy(prefix = prefix.drop(1))
    def getAddress(): String = prefix ++ counter.toString()
  }

  def initState(): BMHState = BMHState("_", 0L, Map.empty, Random.nextLong(), 1.0, 0.0, 1.0)
  def rerunState(samples: Map[String, Value], u: Double, w: Double): BMHState = {
    val keys = samples.keys.toSeq

    if(keys.length > 0) {
      val removeKey = keys(Random.nextInt(keys.length))

      BMHState("_", 0L, samples.removed(removeKey), Random.nextLong(), 1.0, u, w)
    } else {
      BMHState("_", 0L, samples, Random.nextLong(), 1.0, u, w)
    }
  }

  def getState: F[BMHState] = StateT.get
  def setState(s: BMHState): F[Unit] = StateT.set(s)
  def modifyState(f: BMHState => BMHState): F[Unit] = StateT.modify(f)
  def guard(v: Boolean): F[Unit] = StateT.liftF(Option.when(v)( () ))

  def nextAddress(): F[String] = {
    for {
      state <- getState
      address = state.getAddress()
      _ <- setState(state.incCounter())
    } yield address
  }

  def addAddressPrefix(p: String): F[Unit] = {
    modifyState(_.addPrefix(p))
  }

  def popAddressPrefix(): F[Unit] = {
    modifyState(_.popPrefix())
  }

  def sample[T](dist: Dist[T])(implicit domain: Domain[T]): F[T] = {
    for {
      c <- nextAddress()
      state <- getState
      result <- {
        state.samples.get(c) match {
          case None => {
            val seed = state.rngSeed
            val rng = new Random(seed)
            val r = dist.sample(rng)
            val nextSeed = rng.nextLong()
            for(_ <- modifyState(_.copy(rngSeed = nextSeed, samples = state.samples + (c -> domain.encode(r))))) yield r
          }
          case Some(r) => {
            StateT.liftF[Option, EvalState, T](domain.extract(r))
          }
        }
      }
    } yield result
  }

  def observe[T](dist: Dist[T], value: T): F[T] = {
    for {
      state <- getState
      sigma = state.sigma
      p = dist.observe(value)
      _ <- guard(state.u < sigma * p / state.w)
      _ <- modifyState(_.copy(sigma = sigma * p))
    } yield value 
  }

  def if_[T](cond: Boolean, ifTrue: => F[T], ifFalse: => F[T]): F[T] = {
    if(cond) {
      for {
        _ <- addAddressPrefix("t")
        v <- ifTrue
        _ <- popAddressPrefix()
      } yield v
    } else {
      for {
        _ <- addAddressPrefix("f")
        v <- ifFalse
        _ <- popAddressPrefix()
      } yield v
    }
  }

  def pure_[T](v: T): F[T] = StateT.pure(v)

  def sequence_[T, S[_]](fs: S[F[T]])(implicit t: Traverse[S]): F[S[T]] = 
    t.sequence(fs)

  def run[T](prg: F[T], n: Long): Result[T] = {
    val v = prg.run(initState())

    v match {
      case None => Iterable.empty
      case Some(v) => {
        var r = v._2
        var w = v._1.sigma
        var samples = Map.empty[String, Value]

        val builder = Iterable.newBuilder[T]

        (1L until n).foreach { s =>
          prg.run(rerunState(samples, Random.nextDouble(), w)).foreach { vv =>
            w = vv._1.sigma
            r = vv._2
            samples = vv._1.samples
          }

          builder += r
        }

        builder.result().map { v => (v, 1.0) }
      }
    }
  }
}
