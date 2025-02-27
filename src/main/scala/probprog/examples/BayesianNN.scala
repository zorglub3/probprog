package probprog.examples

import probprog._
import probprog.util.DenseMatrix

class BayesianNN(val lang: Language) {
  import lang._
  import DenseMatrix._

  def bayesianNeuralNetwork(): F[Double] = {
    val weightPrior = normal(0.0, 1.0)

    ???
  }
}
