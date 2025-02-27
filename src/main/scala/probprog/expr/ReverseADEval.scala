package probprog.expr

import cats.data.State

class ReverseADEval extends DiffExprEval {
  import ReverseADEval._

  type Env[T] = Map[EnvEntry, T]

  def emptyEnv[T](): Env[T] = Map.empty

  def assign[T](env: Env[T], variable: DiffExpr.Variable[T], value: T): Env[T] = {
    env + (EvalEntry(variable.id) -> value)
  }

  type F[T] = State[Env[T], T]

  def get[T](id: DiffExpr.Id, default: T): F[T] = 
    State.inspect(_.getOrElse(EvalEntry(id), default))
 
  def assignId[T](id: DiffExpr.Id, value: T): F[T] = { 
    for {
      _ <- State.modify[Env[T]] { _.updated(EvalEntry(id), value) }
    } yield value
  }

  def addDiff[T](id: DiffExpr.Id, value: T)(implicit frac: Fractional[T]): F[T] = {
    import frac._
    val entry = DiffEntry(id)

    for {
      s <- State.get[Env[T]]
      v = s.getOrElse(entry, zero) + value
      _ <- State.set[Env[T]](s + (entry -> v))
    } yield v
  }

  def eval[T](env: Env[T], expr: DiffExpr[T])(implicit frac: Fractional[T]): (Env[T], T) = {
    import Operator._
    import DiffExpr._
    import frac._

    def opHelper(op: Operator, l: T, r: T): T = {
      op match {
        case Add => l + r
        case Subtract => l - r
        case Multiply => l * r
        case Divide => l / r
      }
    }

    def helper(e: DiffExpr[T]): F[T] = {
      e match {
        case BinOp(op, l, r) => {
          for {
            lv <- helper(l)
            rv <- helper(r)
            v  = opHelper(op, lv, rv)
            _ <- assignId(e.id, v)
          } yield v
        }
        case Constant(v) => assignId(e.id, v).flatMap(_ => State.pure(v))
        case Variable(_name) => {
          for {
            v <- get(e.id, frac.zero)
            _ <- assignId(e.id, v)
          } yield v
        }
        case FunCall(function, args) => {
          for {
            av <- helper(args(0))
            v  = function.eval(av)
            _ <- assignId(e.id, v)
          } yield v
        }
        case _ => throw new java.lang.RuntimeException(s"Unsupported expression: $e")
      }
    }

    helper(expr).run(env).value
  }

  def diff[T](env: Env[T], expr: DiffExpr[T], amount: T)(implicit frac: Fractional[T]): Env[T] = {
    import DiffExpr._
    import Operator._
    import frac._

    def helper(e: DiffExpr[T], v: T): F[T] = {
      e match {
        case BinOp(Add, l, r) => {
          for {
            _ <- helper(l, v)
            _ <- helper(r, v)
          } yield v
        }
        case BinOp(Subtract, l, r) => {
          for {
            _ <- helper(l, v)
            _ <- helper(r, -v)
          } yield v
        }
        case BinOp(Multiply, l, r) => {
          for {
            lv <- get(l.id, zero)
            rv <- get(r.id, zero)
            _ <- helper(l, rv * v)
            _ <- helper(r, lv * v)
          } yield v
        }
        case BinOp(Divide, l, r) => {
          for {
            lv <- get(l.id, zero)
            rv <- get(r.id, zero)
            rr =  rv * rv
            _  <- helper(l, v * rv / rr)
            _  <- helper(r, v * lv / rr)
          } yield v
        }
        case Constant(_) => State.pure(v)
        case Variable(_) => addDiff(e.id, v)
        case FunCall(function, Vector(arg)) => {
          for {
            av <- get(arg.id, zero)
            _  <- helper(arg, function.diff(av) * v)
          } yield v
        }
        case _ => State.pure(v)
      }
    }

    helper(expr, amount).runS(env).value
  }

  import scala.language.implicitConversions

  implicit def variableEntry[T](v: DiffExpr[T]): DiffEntry = DiffEntry(v.id)
}

object ReverseADEval {
  sealed trait EnvEntry

  final case class EvalEntry(id: DiffExpr.Id) extends EnvEntry
  final case class DiffEntry(id: DiffExpr.Id) extends EnvEntry
}
