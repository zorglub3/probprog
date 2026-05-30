package probprog

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import probprog.impl._
import probprog.examples.LibrariansAndFarmers

class LibrariansAndFarmersSpec extends AnyFlatSpec with Matchers {
  "A librarian-farmer check " should "evaluate to 50 change of FARMER" in {
    val prg = new LibrariansAndFarmers(new LikelihoodWeight)

    val result = prg.lang.run(prg.prog, 10000)

    val normalizedResult = util.Normalize(result).toMap

    assert(normalizedResult(LibrariansAndFarmers.FARMER) === 0.65 +- 0.02)
    assert(normalizedResult(LibrariansAndFarmers.LIBRARIAN) === 0.35 +- 0.02)
  }
}


