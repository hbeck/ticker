package evaluation

import core.lars.{LarsProgram, Diamond, LarsProgram$, W}
import fixtures.{ConfigurableEvaluationSpec, TimeTestFixtures, TmsPushEngine}
import org.scalatest.Matchers._
import org.scalatest.OptionValues._
import org.scalatest.Inspectors._

/**
  * Created by FM on 09.06.16.
  */
class TmsPerformanceSample extends ConfigurableEvaluationSpec with TimeTestFixtures with TmsPushEngine {
  val program = LarsProgram.from(
    a <= b,
    b <= c,
    c <= d,
    d <= e,
    e <= f,
    f <= g,
    g <= h,
    h <= i,
    i <= j,
    j <= W(100, Diamond, k)
  )

  "An empty Program" should "lead to an empty model at t0" in {
    evaluationEngine.evaluate(t0).get.value shouldBe empty
  }

  "{1 -> k}" should "lead to model a for 1...100" in {
    evaluationEngine.append(t1)(k)

    forAll(1 to 100) { t => evaluationEngine.evaluate(t).get.value should contain(a) }
  }
}

class AllPerformance extends RunWithAllImplementations(new TmsPerformanceSample)
