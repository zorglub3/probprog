package probprog.expr

import scala.math
import java.lang.RuntimeException

class ForwardEval extends DiffExprEval {
  type Env[T] = Map[DiffExpr.Id, T]

  def emptyEnv[T](): Env[T] = Map.empty

  def assign[T](env: Env[T], variable: DiffExpr.Variable[T], value: T): Env[T] = {
    env + (variable.id -> value)
  }

  def eval[T](env: Env[T], expr: DiffExpr[T])(implicit frac: Fractional[T]): (Env[T], T) = {
    import DiffExpr._
    import Operator._

    val result = expr match {
      case BinOp(Add, l, r) => frac.plus(eval(env, l)._2, eval(env, r)._2)
      case BinOp(Subtract, l, r) => frac.minus(eval(env, l)._2, eval(env, r)._2)
      case BinOp(Multiply, l, r) => frac.times(eval(env, l)._2, eval(env, r)._2)
      case BinOp(Divide, l, r) => frac.div(eval(env, l)._2, eval(env, r)._2)
      case Constant(v) => v
      case e: Variable[T] => env.getOrElse(e.id, frac.zero)
      case FunCall(function, args) => function.eval(eval(env, args(0))._2)
      case _ => frac.zero
    }

    (env, result)
  }

  def diff[T](env: Env[T], expr: DiffExpr[T], amount: T)(implicit frac: Fractional[T]): Env[T] = {
    throw new RuntimeException("No differentiation evaluation for ForwardEval")
  }
}
