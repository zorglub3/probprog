package probprog.examples

import probprog.Language

class FiftyFifty(val lang: Language) {
  import lang._

  def fiftyFifty: F[Boolean] =
    for {
      v <- sample(normal(0.0, 1.0))
      _ <- observe(bernoulli(0.5), 1.0)
    } yield v > 0.0
}
