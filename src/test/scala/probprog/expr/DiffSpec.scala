package probprog.expr

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DiffSpec extends AnyFlatSpec with Matchers {
  import DiffExpr._

  "Simple expressions (u * 3 + 4)" should "evaluate to 10 for u=2" in {
    val evaluator = new ForwardEval

    val v1 = variable[Double]("v1")
    val e1: DiffExpr[Double] = v1 * 3.0 + 4.0

    import evaluator._

    val env = assign(emptyEnv(), v1, 2.0)

    eval(env, e1)._2 shouldBe 10.0
  }

  it should "differentiate to du=3 for u=2" in {
    val evaluator = new ReverseADEval

    val v1 = variable[Double]("v1")
    val e1: DiffExpr[Double] = v1 * 3.0 + 4.0

    import evaluator._

    val env = assign(emptyEnv(), v1, 2.0)

    val (env2, result) = eval(env, e1)

    result shouldBe 10.0

    val env3 = diff(env2, e1, 1.0)

    env3.get(v1) shouldBe Some(3.0)
  }

  "Expression with division (10 / v - v)" should "differentiate to dv=1.5 for v=2" in {
    val evaluator = new ReverseADEval

    val v1 = variable[Double]("v")
    val e1: DiffExpr[Double] = 10.0 / v1 - v1

    import evaluator._

    val env = assign(emptyEnv(), v1, 2.0)

    val (env2, result) = eval(env, e1)

    result shouldBe 3.0

    val env3 = diff(env2, e1, 1.0)

    env3.get(v1) shouldBe Some(1.5)
  }
}


