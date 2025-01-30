package probprog

trait Value

object Value {
  case class DoubleValue(v: Double) extends Value
  case class IntegerValue(v: Int) extends Value
  case class CharValue(c: Char) extends Value
  case class StringValue(s: String) extends Value
}

trait Domain[T] {
  def extract(v: Value): Option[T]
  def encode(v: T): Value
}
