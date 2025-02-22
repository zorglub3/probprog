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

object Domain {
  implicit object DoubleDomain extends Domain[Double] {
    def extract(v: Value): Option[Double] = {
      v match {
        case Value.DoubleValue(v) => Some(v)
        case _ => None
      }
    }

    def encode(v: Double): Value = Value.DoubleValue(v)
  }

  implicit object IntegerDomain extends Domain[Int] {
    def extract(v: Value): Option[Int] = {
      v match {
        case Value.IntegerValue(v: Int) => Some(v)
        case _ => None
      }
    }

    def encode(v: Int): Value = Value.IntegerValue(v)
  }

  implicit object CharDomain extends Domain[Char] {
    def extract(v: Value): Option[Char] = {
      v match {
        case Value.CharValue(c) => Some(c)
        case _ => None
      }
    }

    def encode(v: Char): Value = Value.CharValue(v)
  }

  implicit object StringDomain extends Domain[String] {
    def extract(v: Value): Option[String] = {
      v match {
        case Value.StringValue(s) => Some(s)
        case _ => None
      }
    }

    def encode(s: String): Value = Value.StringValue(s)
  }
}
