package probprog.util

object DenseMatrix {
  type Mat[T] = Vector[Vector[T]]

  def id[T](n: Int)(implicit frac: Fractional[T]): Mat[T] =
    Vector.tabulate(n, n) { case (r, c) =>
      if(r == c) {
        frac.one
      } else {
        frac.zero
      }
    }

  implicit class MatSyntax[T](mat: Mat[T]) {
    def rows = mat.length
    def cols = if(mat.length == 0) { 0 } else { mat(0).length }

    def apply(r: Int, c: Int): T = mat(r)(c)

    def map[U](f: T => U): Mat[U] = mat.map(_.map(f))

    def mulRowCol(m2: Mat[T], r: Int, c: Int)(implicit frac: Fractional[T]): T = {
      import frac._

      (0 until cols).map { idx => mat(r, idx) * m2(idx, c) } .sum
    }

    def matMul(m2: Mat[T])(implicit frac: Fractional[T]): Mat[T] = {
      Vector.tabulate(rows, m2.cols) { case (r, c) =>
        mulRowCol(m2, r, c)
      }
    }

    def repeat(rs: Int, cs: Int): Mat[T] = {
      Vector.tabulate(rows * rs, cols * cs) { case (r, c) =>
        mat(r % rows, c % cols)
      }
    }

    def matAdd(m2: Mat[T])(implicit frac: Fractional[T]): Mat[T] = {
      import frac._

      Vector.tabulate(rows, cols) { case (r, c) =>
        mat(r, c) + m2(r, c)
      }
    }
  }
}


