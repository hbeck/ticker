package evaluation

import core.lars.{Diamond, LarsProgram, W}
import engine.asp.tms.policies.LazyRemovePolicy
import engine.config.BuildEngine
import fixtures.{ConfigurableEvaluationSpec, EvaluationEngineBuilder, TimeTestFixtures, TmsPushEngine}
import jtms.JtmsExtended
import org.scalatest.Matchers._
import org.scalatest.OptionValues._
import org.scalatest.Inspectors._

import scala.util.Random

/**
  * Created by FM on 09.06.16.
  */
class TmsPerformanceSample extends ConfigurableEvaluationSpec with TimeTestFixtures with EvaluationEngineBuilder {
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
  val defaultEngine = (p: LarsProgram) => BuildEngine.withProgram(p).useAsp().withTms().usingPolicy(LazyRemovePolicy(JtmsExtended(new Random(1)), 10)).use().usePush().start()

  "An empty Program" should "lead to an empty model at t0" in {
    evaluationEngine.evaluate(t0).get.value shouldBe empty
  }

  "{1 -> k}" should "lead to model a for 1...100" in {
    evaluationEngine.append(t1)(k)

    forAll(1 to 100) { t => evaluationEngine.evaluate(t).get.value should contain(a) }
  }
}

class AllPerformance extends RunWithAllImplementations(new TmsPerformanceSample)
