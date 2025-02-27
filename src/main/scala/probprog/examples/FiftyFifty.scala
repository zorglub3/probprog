package probprog.examples

import probprog.Language

class FiftyFifty(val lang: Language) {
  import lang._

  def prog: F[Double] =
    for {
      v <- sample(bernoulli(0.5))
      _ <- observe(bernoulli(0.5), v)
    } yield v 
}
