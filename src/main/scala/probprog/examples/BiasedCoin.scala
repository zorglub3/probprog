package probprog.examples

import probprog.Language

class BiasedCoin(val lang: Language) {
  import lang._

  def biasedCoin: F[Boolean] =
    for {
      v <- sample(normal(0.5, 0.3))
      _ <- observe(bernoulli(v), 0.0)
    } yield v > 0.6
}
