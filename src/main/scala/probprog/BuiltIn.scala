package probprog

sealed trait BuiltIn[T1, T2]

object BuiltIn {
  case object NormalDist extends BuiltIn[Double, Distribution.Normal]
  case object IntAdd extends BuiltIn[Integer, Integer]
  case object BoolAnd extends BuiltIn[Boolean, Boolean]
  case object BoolOr extends BuiltIn[Boolean, Boolean]
  case object DoubleAdd extends BuiltIn[Double, Double]
  case object DoubleMul extends BuiltIn[Double, Double]

  case class ToVec[T]() extends BuiltIn[T, Vector[T]]
}
