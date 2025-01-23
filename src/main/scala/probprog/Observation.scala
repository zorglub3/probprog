package probprog

import scala.collection.mutable.ListBuffer

class ExprCollection {
  val observations = ListBuffer.empty[Expr.Observe[_, _]]
  val samples = ListBuffer.empty[Expr.Sample[_, _]]

  def addObservation[T1, T2](
    expr: Expr.Observe[T1, T2],
    ): Unit = {
      observations += expr
  }

  def addSample[T1, T2](
    expr: Expr.Sample[T1, T2],
    ): Unit = {
      samples += expr
  }
}
