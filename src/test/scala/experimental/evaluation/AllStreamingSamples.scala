package experimental.evaluation

import engine.EngineStreamSpec
import engine.examples.{XWindowBoxASample, YWindowDiamondASample, ZWindowTimeASample}
import fixtures._
import org.scalatest.{Spec, Suite, Suites}

/**
  * Created by FM on 01.06.16.
  */
abstract class AllStreamingSamples extends Suites(
  // TODO: add all specs here
  new EngineStreamSpec,
  new ZWindowTimeASample,
  new XWindowBoxASample,
  new YWindowDiamondASample,

  new StratifiedSample,
  new JtmsEssenceSimpleWindowSample
) with ConfigurableEvaluationSuite


class AspPullClingo extends AllStreamingSamples with ClingoPullEngine

class AspPushClingo extends AllStreamingSamples with ClingoPushEngine

class AspPushTms extends AllStreamingSamples with TmsDirectPolicyEngine

class LazyRemoveTms extends AllStreamingSamples with JtmsGreedyLazyRemovePolicyEngine

class RunWithAllImplementations[TSpec <: ConfigurableEvaluationSpec](spec: TSpec) extends Spec {

  class SingleClingoPullTest extends Suites(spec) with ConfigurableEvaluationSuite with ClingoPullEngine

  class SingleClingoPushTest extends Suites(spec) with ConfigurableEvaluationSuite with ClingoPushEngine

  class SingleDirectTmsTest extends Suites(spec) with ConfigurableEvaluationSuite with TmsDirectPolicyEngine

  class SingleLazyRemoveTmsTest extends Suites(spec) with ConfigurableEvaluationSuite with JtmsGreedyLazyRemovePolicyEngine

  override val nestedSuites: collection.immutable.IndexedSeq[Suite] = Vector.empty ++ Seq(new SingleClingoPushTest, new SingleClingoPullTest, new SingleDirectTmsTest, new SingleLazyRemoveTmsTest)

}

