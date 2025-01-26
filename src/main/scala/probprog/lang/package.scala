package probprog

package object lang {
  import probprog._
  import scala.language.implicitConversions

  implicit def intConstant(v: Int): Expr[Integer] =
    Expr.Constant[Integer](v)

  implicit def boolConstant(v: Boolean): Expr[Boolean] =
    Expr.Constant[Boolean](v)

  implicit def doubleConstant(v: Double): Expr[Double] =
    Expr.Constant[Double](v)

  implicit object AddIntegers extends AddEvidence[Integer, Integer, Integer] {
    def add(e1: Expr[Integer], e2: Expr[Integer]): Expr[Integer] = {
      Expr.Primitive(BuiltIn.IntAdd, Vector(e1, e2))
    }
  }

  implicit object AddDouble extends AddEvidence[Double, Double, Double] {
    def add(e1: Expr[Double], e2: Expr[Double]): Expr[Double] = {
      Expr.Primitive(BuiltIn.DoubleAdd, Vector(e1, e2))
    }
  }

  implicit object MulDouble extends MulEvidence[Double, Double, Double] {
    def mul(e1: Expr[Double], e2: Expr[Double]): Expr[Double] = {
      Expr.Primitive(BuiltIn.DoubleMul, Vector(e1, e2))
    }
  }

  implicit object NormalDist extends DistEvidence[Distribution.Normal, Double] {
    def toDist(e: Expr[Distribution.Normal]): Expr[Distribution[Double]] = 
      e.asInstanceOf[Expr[Distribution[Double]]]
  }

  def normal(expected: Expr[Double], variance: Expr[Double]): Expr[Distribution.Normal] = {
    Expr.Primitive(BuiltIn.NormalDist, Vector(expected, variance))
  }

  def vector[T](args: Expr[T]*): Expr[Vector[T]] = {
    Expr.Primitive(BuiltIn.ToVec(), args.toVector)
  }
}
