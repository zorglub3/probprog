package probprog.expr

import scala.math

class ReverseADEval extends DiffExprEval {
  import ReverseADEval._
  type Env[T] = Map[EnvEntry, T]

  def emptyEnv[T](): Env[T] = Map.empty

  def assign[T](env: Env[T], variable: DiffExpr.Variable[T], value: T): Env[T] = {
    env + (EvalEntry(variable.id) -> value)
  }

  def get[T](env: Env[T], id: DiffExpr.Id, default: T): T = 
    env.getOrElse(EvalEntry(id), default)

  def assignId[T](env: Env[T], id: DiffExpr.Id, value: T): Env[T] = {
    env + (EvalEntry(id) -> value)
  }

  def addId[T](env: Env[T], id: DiffExpr.Id, value: T)(implicit frac: Fractional[T]): Env[T] = {
    import frac._
    env + (DiffEntry(id) -> (env.getOrElse(DiffEntry(id), frac.zero) + value))
  }

  def eval[T](env: Env[T], expr: DiffExpr[T])(implicit frac: Fractional[T]): (Env[T], T) = {
    import DiffExpr._
    import Operator._
    import frac._

    val (newEnv, result) = expr match {
      case BinOp(op, l, r) => {
        val (env1, res1) = eval(env, l)
        val (env2, res2) = eval(env1, l)

        op match {
          case Add => (env2, res1 + res2)
          case Subtract => (env2, res1 - res2)
          case Multiply => (env2, res1 * res2)
          case Divide => (env2, res1 / res2)
        }
      }
      case Constant(v) => (env, v)
      case e: Variable[T] => (env, env.getOrElse(EvalEntry(e.id), frac.zero))
      case FunCall(function, args) => {
        val (env1, arg1) = eval(env, args(0))
        (env1, function.eval(arg1))
      }
      case _ => (env, frac.zero)
    }

    (assignId(newEnv, expr.id, result), result)
  }

  def diff[T](env: Env[T], expr: DiffExpr[T], amount: T)(implicit frac: Fractional[T]): Env[T] = {
    import DiffExpr._
    import Operator._
    import frac._

    expr match {
      case BinOp(Add, l, r) => {
        val env1 = diff(env, l, amount)
        diff(env1, r, amount)
      }
      case BinOp(Subtract, l, r) => {
        val env1 = diff(env, l, amount)
        diff(env1, r, frac.minus(frac.zero, amount))
      }
      case BinOp(Multiply, l, r) => {
        val env1 = diff(env, l, frac.times(amount, get(env, r.id, frac.zero)))
        diff(env1, r, frac.times(amount, get(env, l.id, frac.zero)))
      }
      case BinOp(Divide, l, r) => {
        val l1 = get(env, l.id, frac.zero)
        val r1 = get(env, r.id, frac.zero)
        val rr = frac.times(r1, r1)

        val env1 = diff(env, l, amount * r1 / rr)
        diff(env1, r, amount * l1 / rr)
      }
      case Constant(v) => env
      case e: Variable[T] => addId(env, e.id, amount)
      case FunCall(function, Vector(arg)) => {
        diff(env, arg, function.diff(get(env, arg.id, frac.zero)) * amount)
      } 
      case _ => env
    }
  }
}

object ReverseADEval {
  sealed trait EnvEntry

  final case class EvalEntry(id: DiffExpr.Id) extends EnvEntry
  final case class DiffEntry(id: DiffExpr.Id) extends EnvEntry
}
