package probprog

import cats.data.IndexedStateT
import scala.util.Random

class BufferedMetropolisHastings extends Language[Option] { 
  type EvalState = BMHState 

  case class BMHState(
    prefix: String, 
    counter: Long,
    samples: Map[String, Double],
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
  def rerunState(samples: Map[String, Double], u: Double, w: Double): BMHState = {
    val keys = samples.keys.toSeq

    if(keys.length > 0) {
      val removeKey = keys(Random.nextInt(keys.length))

      BMHState("_", 0L, samples.removed(removeKey), Random.nextLong(), 1.0, u, w)
    } else {
      BMHState("_", 0L, samples, Random.nextLong(), 1.0, u, w)
    }
  }

  def getState: F[BMHState] = IndexedStateT.get
  def setState(s: BMHState): F[Unit] = IndexedStateT.set(s)
  def modifyState(f: BMHState => BMHState): F[Unit] = IndexedStateT.modify(f)
  def guard(v: Boolean): F[Unit] = IndexedStateT.liftF(Option.when(v)( () ))

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

  def sample(dist: Distribution): F[Double] = {
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
            for(_ <- modifyState(_.copy(rngSeed = nextSeed, samples = state.samples + (c -> r)))) yield r
          }
          case Some(r) => {
            IndexedStateT.pure[Option, BMHState, Double](r)
          }
        }
      }
    } yield result
  }

  def observe(dist: Distribution, value: Double): F[Double] = {
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

  def sequence_[T](fs: Iterable[F[T]]): F[Unit] = ???

  def run[T](prg: F[T], n: Long): Result[T] = {
    val v = prg.run(initState())

    v match {
      case None => MHResult(Vector.empty)
      case Some(v) => {
        var r = v._2
        var w = v._1.sigma
        var samples = Map.empty[String, Double]

        val builder = Vector.newBuilder[T]

        (1L until n).foreach { s =>
          prg.run(rerunState(samples, Random.nextDouble(), w)).foreach { vv =>
            w = vv._1.sigma
            r = vv._2
            samples = vv._1.samples
          }

          builder += r
        }

        MHResult(builder.result())
      }
    }
  }
}
