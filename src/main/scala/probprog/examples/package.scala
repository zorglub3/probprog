package probprog

package object examples {
  import probprog.lang._

  def bayesianLinearRegression(implicit exprs: ExprCollection): Expr[Vector[Double]] = {
    def observeData(
      slope: Expr[Double], 
      intercept: Expr[Double],
      x: Expr[Double],
      y: Expr[Double]
    ): Expr[Double] = {
      val fx = slope * x + intercept
      normal(fx, 1.0).observe(y)
    }

    val slope = normal(0.0, 1.0).sample()
    val intercept = normal(0.0, 1.0).sample()

    val y1 = observeData(slope, intercept, 1.0, 2.1)
    val y2 = observeData(slope, intercept, 2.0, 3.9)
    val y3 = observeData(slope, intercept, 3.0, 5.3)
    val y4 = observeData(slope, intercept, 4.0, 7.7)
    val y5 = observeData(slope, intercept, 5.0, 10.2)

    vector(slope, intercept)
  }
}
