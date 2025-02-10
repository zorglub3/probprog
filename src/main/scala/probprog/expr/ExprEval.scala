package probprog.expr

trait DiffExprEval {
  type Env[T]

  def emptyEnv[T](): Env[T]
  def assign[T](env: Env[T], variable: DiffExpr.Variable[T], value: T): Env[T]

  def eval[T](env: Env[T], expr: DiffExpr[T])(implicit frac: Fractional[T]): (Env[T], T)
  def diff[T](env: Env[T], expr: DiffExpr[T], amount: T)(implicit frac: Fractional[T]): Env[T]
}
