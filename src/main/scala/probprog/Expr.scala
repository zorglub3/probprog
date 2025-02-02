package probprog

import scala.language.implicitConversions

sealed trait Expr[T] {
  def +(v: Expr[T]): Expr[T] = new Expr.BinOp(Operator.Add, this, v)
}

object Expr {
  final class BinOp[T](operator: Operator, left: Expr[T], right: Expr[T]) extends Expr[T]
  final class Constant[T](value: T) extends Expr[T]
  final class Variable[T](name: String) extends Expr[T]

  implicit def constant[T](c: T): Expr[T] = new Constant[T](c)
}

sealed trait Operator

object Operator {
  case object Add extends Operator
  case object Subtract extends Operator
  case object Multiply extends Operator
  case object Divide extends Operator
}

