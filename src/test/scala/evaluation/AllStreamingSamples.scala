package evaluation

import engine.EngineStreamSpec
import fixtures.{ClingoPullEngine, ClingoPushEngine, ConfigurableEvaluationSuite, TmsPushEngine}
import org.scalatest.Suites

/**
  * Created by FM on 01.06.16.
  */
abstract class AllStreamingSamples extends Suites(
  // TODO: add all specs here
  new EngineStreamSpec
) with ConfigurableEvaluationSuite


class AspPullClingo extends AllStreamingSamples with ClingoPullEngine
class AspPushClingo extends AllStreamingSamples with ClingoPushEngine
class AspPushTms extends AllStreamingSamples with TmsPushEngine

