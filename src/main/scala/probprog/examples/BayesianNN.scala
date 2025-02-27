package probprog.examples

import probprog._

class BayesianNN(val lang: Language) {
  import lang._

  def bayesianNeuralNetwork(): F[Double] = {
    val weightPrior = normal(0.0, 1.0)

    ???
  }
}
