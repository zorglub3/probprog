package probprog

import scala.collection.mutable.ListBuffer

class ExprCollection {
  val observations = ListBuffer.empty[Expr.Observe[_, _]]
  val samples = ListBuffer.empty[Expr.Sample[_, _]]

  def addObservation[T2, T1 <: Distribution[T2]](
    expr: Expr.Observe[T2, T1],
  ): Unit = {
      observations += expr
  }

  def addSample[T2, T1 <: Distribution[T2]](
    expr: Expr.Sample[T2, T1],
  ): Unit = {
      samples += expr
  }
}
