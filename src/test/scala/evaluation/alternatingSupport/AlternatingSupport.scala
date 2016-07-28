package evaluation.alternatingSupport

import core.lars.TimePoint
import engine.Result
import engine.asp.{GroundRule, PlainLarsToAsp}
import engine.asp.tms.{GroundRule, GroundedNormalRule, PinnedAspToIncrementalAsp, TmsEvaluationEngine}
import engine.asp.tms.policies.{ImmediatelyAddRemovePolicy, LazyRemovePolicy, TmsPolicy}
import engine.config.{BuildEngine, EngineEvaluationConfiguration}
import fixtures.{ConfigurableEvaluationSpec, TimeTestFixtures, TmsLazyRemovePolicyEngine}
import jtms.JtmsExtended
import org.scalatest.FlatSpec
import org.scalatest.Matchers._

import scala.util.Random

/**
  * Created by FM on 22.07.16.
  */
class AlternatingSupport extends FlatSpec with AlternatingSupportSpec with TimeTestFixtures {


  val transformedProgram = PlainLarsToAsp(program)

  "Two streaming elements and the normal TMS-Evaluation" should "lead to an expensive update" in {
    val tms = JtmsExtended(new Random(1))
    val engine = TmsEvaluationEngine(transformedProgram, LazyRemovePolicy(tms,9))

    engine.append(t1)(b)
    engine.append(t2)(c)

    tms.ancestors(a) should contain(b(t1))

    // at t12 support of a should change to c
    // this should be "expensive" as in the state of a should be set to unknown and the TMS should be forced to recompute it
    engine.evaluate(12)

    tms.ancestors(a) should contain(c(t2))
  }
}
