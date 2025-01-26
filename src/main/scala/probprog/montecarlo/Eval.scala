package probprog.montecarlo

import scala.util.Random
import scala.math.log
import probprog.Expr

class Eval(rnd: Random) {
  type R[T] = (T, Double)

  def apply[T](e: Expr[T], sigma: Double): R[T] = {
    e match {
      case Expr.Constant(v) => (v, sigma)
      case Expr.If(cond, e1, e2) => {
        val r1 = apply(cond, sigma)
        if(r1._1) {
          apply(e1, r1._2)
        } else {
          apply(e2, r1._2)
        }
      }
      case Expr.Primitive(op, args) => ???
      case Expr.Sample(e) => {
        val d = apply(e, sigma)
        (d._1.sample(rnd), d._2)
      }
      case Expr.Observe(e1, e2) => {
        val d = apply(e1, sigma)
        val v = apply(e2, d._2)
        val p = d._1.observe(v._1)
        (v._1, v._2 + log(p))
      }
    }
  }
}
