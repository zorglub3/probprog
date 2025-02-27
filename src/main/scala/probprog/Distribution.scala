package probprog

import scala.util.Random

trait Distribution[T] {
  def sample(rnd: Random): T 
  def observe(v: T): Double

  def sampleN(n: Int, rnd: Random): Seq[T] = {
    val builder = Seq.newBuilder[T]

    for(_ <- 0 until n) { builder += sample(rnd) }

    builder.result()
  }
}
