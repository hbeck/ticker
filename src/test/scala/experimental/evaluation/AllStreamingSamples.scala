package experimental.evaluation

import reasoner.EngineStreamSpec
import reasoner.examples.{XWindowBoxASample, YWindowDiamondASample, ZWindowTimeASample}
import fixtures._
import org.scalatest.{Spec, Suite, Suites}

/**
  * Created by FM on 01.06.16.
  */
abstract class AllStreamingSamples extends Suites(
  new EngineStreamSpec,
  new ZWindowTimeASample,
  new XWindowBoxASample,
  new YWindowDiamondASample,

  new StratifiedSample,
  new JtmsEssenceSimpleWindowSample
) with ConfigurableEvaluationSuite


class AspPullClingo extends AllStreamingSamples with ClingoPullEngine

class AspPushClingo extends AllStreamingSamples with ClingoPushEngine

class RunWithAllImplementations[TSpec <: ConfigurableEngineSpec](spec: TSpec) extends Spec {

  class SingleClingoPullTest extends Suites(spec) with ConfigurableEvaluationSuite with ClingoPullEngine

  class SingleClingoPushTest extends Suites(spec) with ConfigurableEvaluationSuite with ClingoPushEngine

  class SingleIncrementalTest extends Suites(spec) with ConfigurableEvaluationSuite with JtmsIncrementalEngine

  override val nestedSuites: collection.immutable.IndexedSeq[Suite] = Vector.empty ++ Seq(new SingleClingoPushTest, new SingleClingoPullTest, new SingleIncrementalTest)

}

