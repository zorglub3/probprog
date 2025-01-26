package probprog

trait AddEvidence[T1, T2, T3] {
  def add(e1: Expr[T1], e2: Expr[T2]): Expr[T3]
}

trait DistEvidence[T1, T2] {
  def toDist(v: Expr[T1]): Expr[Distribution[T2]]
}

trait MulEvidence[T1, T2, T3] {
  def mul(e1: Expr[T1], e2: Expr[T2]): Expr[T3]
}
