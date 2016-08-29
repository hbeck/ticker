package evaluation.bit

import core.Predicate
import fixtures._
import org.scalatest.Matchers._
import org.scalatest.{Spec, Suite, Suites}

/**
  * Created by FM on 20.06.16.
  */
class AllBitEncodingSampleVariants extends ConfigurableEvaluationSpec with TimeTestFixtures with JtmsLearnLazyRemovePolicyEngine with BitProgram {

  val id = Predicate("id")
  val program = groundLarsProgram()

  "shooting from the hip" should "lead to less losses" in {
    evaluationEngine.evaluate(t0).get.get should not contain (id(7))
  }

}

class RunWithAllImplementations[TSpec <: ConfigurableEvaluationSpec](spec: TSpec) extends Spec {

//  class SingleClingoPullTest extends Suites(spec) with ConfigurableEvaluationSuite with ClingoPullEngine
//  class SingleClingoPushTest extends Suites(spec) with ConfigurableEvaluationSuite with ClingoPushEngine
//  class SingleDirectTmsTest extends Suites(spec) with ConfigurableEvaluationSuite with TmsDirectPolicyEngine

  //greedy
  class SingleLazyRemoveGreedyTest extends Suites(spec) with ConfigurableEvaluationSuite with JtmsGreedyLazyRemovePolicyEngine

  //learn
  class SingleLazyRemoveLearnTest extends Suites(spec) with ConfigurableEvaluationSuite with JtmsLearnLazyRemovePolicyEngine

  override val nestedSuites: collection.immutable.IndexedSeq[Suite] = Vector.empty ++ Seq(new SingleLazyRemoveGreedyTest, new SingleLazyRemoveLearnTest)

}

class AllBitProgramVariants extends RunWithAllImplementations(new AllBitEncodingSampleVariants)