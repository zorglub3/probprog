package probprog.examples

import probprog.Language

class LibrariansAndFarmers(val lang: Language) {
  import lang._
  import LibrariansAndFarmers._

  def prog: F[Int] =
    for {
      v <- sample(librarianOrFarmer)
      d =  if(v == LIBRARIAN) { LIBRARIAN_DISTRIBUTION } else { FARMER_DISTRIBUTION }
      _ <- observe(weightedSequence(d), SHY)
    } yield v

  def librarianOrFarmer = weightedSequence(Seq(LIBRARIAN -> 5.0, FARMER -> 100.0))
}

object LibrariansAndFarmers {
  val FARMER = 0
  val LIBRARIAN = 1

  val SHY = 2
  val NON_SHY = 3

  val LIBRARIAN_DISTRIBUTION = Seq(SHY -> 1.0)
  val FARMER_DISTRIBUTION = Seq(SHY -> 0.1, NON_SHY -> 0.9)
}
