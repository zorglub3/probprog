package probprog.examples

import probprog.Language

class LinearRegression(val lang: Language) {
  import lang._

  def linearRegression(points: Seq[(Double, Double)]): F[(Double, Double)] = {
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
