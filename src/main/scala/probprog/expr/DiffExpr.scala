package probprog.expr

import scala.language.implicitConversions

sealed trait DiffExpr[T] {
  val id: DiffExpr.Id = DiffExpr.incCounter()

  def +(v: DiffExpr[T]): DiffExpr[T] = new DiffExpr.BinOp(Operator.Add, this, v)
  def -(v: DiffExpr[T]): DiffExpr[T] = new DiffExpr.BinOp(Operator.Subtract, this, v)
  def *(v: DiffExpr[T]): DiffExpr[T] = new DiffExpr.BinOp(Operator.Multiply, this, v)
  def /(v: DiffExpr[T]): DiffExpr[T] = new DiffExpr.BinOp(Operator.Divide, this, v)
}

object DiffExpr {
  final class BinOp[T](val operator: Operator, val left: DiffExpr[T], val right: DiffExpr[T]) extends DiffExpr[T]
  final class Constant[T](val value: T) extends DiffExpr[T]
  final class Variable[T](val name: String) extends DiffExpr[T]
  final class FunCall[T](val function: Function1[T], val args: Vector[DiffExpr[T]]) extends DiffExpr[T]

  object BinOp {
    def unapply[T](expr: DiffExpr[T]): Option[(Operator, DiffExpr[T], DiffExpr[T])] = {
      expr match {
        case e: BinOp[T] => Some((e.operator, e.left, e.right))
        case _ => None
      }
    }
  }

  object Constant {
    def unapply[T](expr: DiffExpr[T]): Option[T] = {
      expr match {
        case e: Constant[T] => Some(e.value)
        case _ => None
      }
    }
  }

  object FunCall {
    def unapply[T](expr: DiffExpr[T]): Option[(Function1[T], Vector[DiffExpr[T]])] = {
      expr match {
        case e: FunCall[T] => Some((e.function, e.args))
        case _ => None
      }
    }
  }

  implicit def constant[T](c: T): DiffExpr[T] = new Constant[T](c)

  type Id = Int

  private var counter: Id = 0
  private def incCounter(): Id = this.synchronized {
    counter += 1
    counter
  }
}

sealed trait Operator

object Operator {
  case object Add extends Operator
  case object Subtract extends Operator
  case object Multiply extends Operator
  case object Divide extends Operator
}

sealed trait Function1[T] {
  def eval(arg: T): T
  def diff(arg: T): T
}

object Function1 {
  case object Sin extends Function1[Double] {
    def eval(arg: Double): Double = math.sin(arg)
    def diff(arg: Double): Double = math.cos(arg)
  }

  case object Cos extends Function1[Double] {
    def eval(arg: Double): Double = math.cos(arg)
    def diff(arg: Double): Double = -math.sin(arg)
  }

  case object Exp extends Function1[Double] {
    def eval(arg: Double): Double = math.exp(arg)
    def diff(arg: Double): Double = math.exp(arg)
  }

  case object Log extends Function1[Double] {
    def eval(arg: Double): Double = math.log(arg)
    def diff(arg: Double): Double = 1.0 / arg
  }
}
