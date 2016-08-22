package engine.examples

import core.lars._
import fixtures._
import org.scalatest.Matchers._
import org.scalatest.Inspectors._
import org.scalatest.OptionValues._

/**
  * Created by FM on 12.08.16.
  */
class ATupleWindowDiamondSample extends ConfigurableEvaluationSpec with TimeTestFixtures with TmsLazyRemovePolicyEngine {

  /**
    *
    * ******** 7     10  11      14
    * ---------|------|--|--------|----
    * ******** d      f  e        d
    */
  val program = LarsProgram.from(
    a <= WindowAtom(SlidingTupleWindow(2), Diamond, d)
  )

  "An empty program" should "not lead to a at 0" in {
    evaluationEngine.evaluate(t0).get.value shouldNot contain(a)
  }

  "{7 -> d}" should "lead to a from 7 to 9" in {
    evaluationEngine.append(7)(d)

    forAll(7 to 9) { i =>
      evaluationEngine.evaluate(i).get.value should contain(a)
    }
  }

  "{7 -> d, 10 -> f}" should "still lead to a at 10" in {
    evaluationEngine.append(7)(d)
    evaluationEngine.append(10)(f)

    evaluationEngine.evaluate(10).get.value should contain(a)
  }

  "{7 -> d, 10 -> f, 11 -> e}" should "not lead to a from 11 to 14" in {
    evaluationEngine.append(7)(d)
    evaluationEngine.append(10)(f)
    evaluationEngine.append(11)(e)

    forAll(11 to 14) { i =>
      evaluationEngine.evaluate(i).get.value shouldNot contain(a)
    }
  }

  "{7 -> d, 10 -> f, 11 -> e, 14 -> d}" should "lead to a from 14" in {
    evaluationEngine.append(7)(d)
    evaluationEngine.append(10)(f)
    evaluationEngine.append(11)(e)
    evaluationEngine.append(14)(d)

    evaluationEngine.evaluate(14).get.value should contain(a)
  }

}
