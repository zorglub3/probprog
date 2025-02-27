package probprog

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import probprog.impl._
import probprog.examples.FiftyFifty

class FiftyFiftySpec extends AnyFlatSpec with Matchers {
  "A likelihood-weight evaluator" should "evaluate to 50-50" in {
    val prg = new FiftyFifty(new LikelihoodWeight)

    val result = prg.lang.run(prg.prog, 10000)

    println(s"result: $result")
    val average = new util.Average[Double]

    assert(average.weighted(result) === 0.5 +- 0.1)
  }

  "A Independent Metropolis Hastings evaluator" should "evaluate to 50-50" in {
    val prg = new FiftyFifty(new IndependentMetropolisHastings)

    val result = prg.lang.run(prg.prog, 10000)

    val average = new util.Average[Double]

    assert(average.weighted(result) === 0.5 +- 0.1)
  }

  "A Buffered Metropolis Hastings evaluator" should "evaluate to 50-50" in {
    val prg = new FiftyFifty(new BufferedMetropolisHastings)

    val result = prg.lang.run(prg.prog, 10000)

    val average = new util.Average[Double]

    assert(average.weighted(result) === 0.5 +- 0.1)
  }

  "A parallelized likelihood-weight evaulator" should "evaluate to 50-50" in {
    val prg = new FiftyFifty(new Parallelize(10, new LikelihoodWeight))

    val result = prg.lang.run(prg.prog, 10000)

    val average = new util.Average[Double]

    assert(average.weighted(result) === 0.5 +- 0.1)
  }
}
