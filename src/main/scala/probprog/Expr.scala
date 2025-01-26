package probprog

sealed trait Expr[T] {
  def +[T2, T3](other: Expr[T2])(implicit e: AddEvidence[T, T2, T3]): Expr[T3] = {
    e.add(this, other)
  }

  def *[T2, T3](other: Expr[T2])(implicit e: MulEvidence[T, T2, T3]): Expr[T3] = {
    e.mul(this, other)
  }

  def sample[T2]()(implicit e: DistEvidence[T, T2]): Expr[T2] = {
    Expr.Sample(e.toDist(this))
  }

  def observe[T2](v: Expr[T2])(implicit e: DistEvidence[T, T2], exprs: ExprCollection): Expr[T2] = {
    val expr = Expr.Observe(e.toDist(this), v)
    exprs.addObservation(expr)
    expr
  }
}

object Expr {
  final case class Constant[T](
    value: T,
  ) extends Expr[T]

  /*
  // Not needed - use std Scala variables
  final case class Variable[T](
    name: String,
  ) extends Expr[T]
  */

  /*
  // Not needed - we use the Scala `val v = e1 ; e2`
  final case class Let[T1, T2](
    name: String, 
    e1: Expr[T1], 
    e2: Expr[T2],
  ) extends Expr[T2](probType)
  */

  final case class If[T](
    cond: Expr[Boolean], 
    e1: Expr[T], 
    e2: Expr[T],
  ) extends Expr[T]

  final case class Primitive[T1, T2](
    builtIn: BuiltIn[T1, T2], 
    args: Vector[Expr[T1]],
  ) extends Expr[T2]

  final case class Sample[T1, T2 <: Distribution[T1]](
    e: Expr[T2],
  ) extends Expr[T1]

  final case class Observe[T1, T2 <: Distribution[T1]](
    e1: Expr[T2], 
    e2: Expr[T1],
  ) extends Expr[T1]
}
