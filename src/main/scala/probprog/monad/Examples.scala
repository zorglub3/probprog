package probprog.monad

import probprog.Language

class Examples(val lang: Language) {
  import lang._

  def biasedCoin: F[Boolean] =
    for {
      v <- sample(normal(0.5, 0.3))
      _ <- observe(flip(v), false)
    } yield v > 0.6
}
