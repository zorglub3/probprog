package probprog

import cats.data.IndexedStateT
import cats.{FlatMap, Functor, Foldable, Applicative, Traverse}

abstract class ExprLanguage {
  type F[T]
  type Result[T] = Iterable[(T, Double)]


  def normal(mean: Expr[Double], deviation: Expr[Double]): Expr[Distribution.Normal] = ???

  def sample[T](dist: Expr[Distribution[T]])(implicit domain: Domain[T]): F[Expr[T]]
  def observe[T](dist: Expr[Distribution[T]], value: Expr[T]): F[Expr[T]]
  def if_[T](cond: Expr[Boolean], ifTrue: => F[Expr[T]], ifFalse: => F[Expr[T]]): F[Expr[T]]

  def run[T](prog: F[Expr[T]], n: Long): Result[T]
}
