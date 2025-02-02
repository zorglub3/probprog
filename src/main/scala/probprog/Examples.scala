package probprog

import cats.{Functor, FlatMap}

class Examples(val lang: Language) {
  import lang._

  def biasedCoin: F[Boolean] =
    for {
      v <- sample(normal(0.5, 0.3))
      _ <- observe(bernoulli(v), 0.0)
    } yield v > 0.6

  def fiftyFifty: F[Boolean] =
    for {
      v <- sample(normal(0.0, 1.0))
      _ <- observe(bernoulli(0.5), 1.0)
    } yield v > 0.0

  def linearRegression(points: Iterable[(Double, Double)]): F[(Double, Double)] = {
    def observeData(slope: Double, intercept: Double, x: Double, y: Double): F[Double] = {
      val fx = slope * x + intercept
      observe(normal(fx, 1.0), y)
    }

    for {
      slope <- sample(normal(0.0, 10.0))
      intercept <- sample(normal(0.0, 10.0))
      _ <- sequence_(points.map { case (x, y) => observeData(slope, intercept, x, y) })
    } yield (slope, intercept)
  }
}
