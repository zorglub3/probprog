package probprog

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import cats.{Functor, FlatMap}
import probprog.impl._

class FiftyFifty extends AnyFlatSpec with Matchers {
  class FiftyFifty(val lang: Language) {
    import lang._

    def prog = {
      for {
        v <- sample(bernoulli(0.5))
        _ <- observe(bernoulli(0.5), v)
      } yield v
    }
  }

  "A likelihood-weight evaluator" should "evaluate to 50-50" in {
    val prg = new FiftyFifty(new LikelihoodWeight)

    val result = prg.lang.run(prg.prog, 10000)

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
