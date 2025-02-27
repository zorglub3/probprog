package probprog

trait ExprLanguage {
  type F[T]
  type E[T]
  type Dist[T]
  type Result[T] = Iterable[(T, Double)]

  def normal(mean: E[Double], deviation: E[Double]): E[distribution.Normal]

  def sample[T](dist: E[Distribution[T]])(implicit domain: Domain[T]): F[E[T]]
  def observe[T](dist: E[Dist[T]], value: E[T]): F[E[T]]
  def if_[T](cond: E[Boolean], ifTrue: => F[E[T]], ifFalse: => F[E[T]]): F[E[T]]

  def run[T](prog: F[E[T]], n: Long): Result[T]
}
