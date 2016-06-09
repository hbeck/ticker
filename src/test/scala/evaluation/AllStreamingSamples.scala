package evaluation

import engine.EngineStreamSpec
import engine.examples.{XWindowBoxASample, YWindowDiamondASample, ZWindowTimeASample}
import fixtures._
import org.scalatest.{Suite, Suites}

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

class AspPushTms extends AllStreamingSamples with TmsPushEngine

class RunWithAllImplementations[TSpec <: ConfigurableEvaluationSpec](spec: TSpec) extends Suite {

  class SingleClingoPullTest extends Suites(spec) with ConfigurableEvaluationSuite with ClingoPullEngine

  class SingleClingoPushTest extends Suites(spec) with ConfigurableEvaluationSuite with ClingoPushEngine

  class SingleTmsTest extends Suites(spec) with ConfigurableEvaluationSuite with TmsPushEngine

  override val nestedSuites: collection.immutable.IndexedSeq[Suite] = Vector.empty ++ Seq(new SingleClingoPushTest, new SingleClingoPullTest, new SingleTmsTest)


}

