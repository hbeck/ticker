package experimental.eval

import reasoner.ReasonerStreamSpec
import reasoner.examples.{XWindowBoxASample, YWindowDiamondASample, ZWindowTimeASample}
import fixtures._
import org.scalatest.{Spec, Suite, Suites}

/**
  * Created by FM on 01.06.16.
  */
abstract class AllStreamingSamples extends Suites(
  new ReasonerStreamSpec,
  new ZWindowTimeASample,
  new XWindowBoxASample,
  new YWindowDiamondASample,

  new StratifiedSample,
  new JtmsEssenceSimpleWindowSample
) with ConfigurableEvaluationSuite


class AspPullClingo extends AllStreamingSamples with ClingoPullReasoner

class AspPushClingo extends AllStreamingSamples with ClingoPushReasoner

class RunWithAllImplementations[TSpec <: ConfigurableReasonerSpec](spec: TSpec) extends Spec {

  class SingleClingoPullTest extends Suites(spec) with ConfigurableEvaluationSuite with ClingoPullReasoner

  class SingleClingoPushTest extends Suites(spec) with ConfigurableEvaluationSuite with ClingoPushReasoner

  class SingleIncrementalTest extends Suites(spec) with ConfigurableEvaluationSuite with JtmsIncrementalReasoner

  override val nestedSuites: collection.immutable.IndexedSeq[Suite] = Vector.empty ++ Seq(new SingleClingoPushTest, new SingleClingoPullTest, new SingleIncrementalTest)

}

