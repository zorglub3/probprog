package probprog

trait ExprEval {
  type Env[T]

  def emptyEnv[T](): Env[T]
  def assign[T](env: Env[T], variable: Expr.Variable[T], value: T): Env[T]

  def eval[T](env: Env[T], expr: Expr[T])(implicit frac: Fractional[T]): (Env[T], T)
  def diff[T](env: Env[T], expr: Expr[T], amount: T)(implicit frac: Fractional[T]): Env[T]
}
