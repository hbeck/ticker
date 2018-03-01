package reasoner.parser.factories.slidingWindowFunctionFactory

import java.util.concurrent.TimeUnit

import core.lars.{TimeWindow, TimeWindowSize}
import reasoner.parser.wrapper.ParamWrapper
import org.scalatest.FlatSpec

/**
  * Created by et on 22.04.17.
  */
class TimeWindowFactoryTest extends FlatSpec {

  behavior of "SlidingTimeWindowFactoryTest"

  it should "create" in {
    assert(SlidingTimeWindowFactory(List(ParamWrapper(25,Some("sec")))).getWindowFunction == TimeWindow(TimeWindowSize(25,TimeUnit.SECONDS)))
    assert(SlidingTimeWindowFactory(List(ParamWrapper(25,Some("min")))).getWindowFunction == TimeWindow(TimeWindowSize(25,TimeUnit.MINUTES)))
  }

}
