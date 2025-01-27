package probprog

import cats.{Functor, FlatMap}

class Examples[E[_]: Functor : FlatMap](val lang: Language[E]) {
  import lang._

  def biasedCoin: F[Boolean] =
    for {
      v <- sample(normal(0.5, 0.3))
      _ <- observe(flip(v), false)
    } yield v > 0.6

  def fiftyFifty: F[Boolean] =
    for {
      v <- sample(normal(0.0, 1.0))
      _ <- observe(flip(0.5), true)
    } yield v > 0.0
}
