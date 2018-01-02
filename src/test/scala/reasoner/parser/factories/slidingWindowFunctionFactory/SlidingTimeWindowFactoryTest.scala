package reasoner.parser.factories.slidingWindowFunctionFactory

import java.util.concurrent.TimeUnit

import core.lars.{SlidingTimeWindow, TimeWindowSize}
import reasoner.parser.wrapper.ParamWrapper
import org.scalatest.FlatSpec

/**
  * Created by et on 22.04.17.
  */
class SlidingTimeWindowFactoryTest extends FlatSpec {

  behavior of "SlidingTimeWindowFactoryTest"

  it should "create" in {
    assert(SlidingTimeWindowFactory(List(ParamWrapper(25,Some("sec")))).getWindowFunction == SlidingTimeWindow(TimeWindowSize(25,TimeUnit.SECONDS)))
    assert(SlidingTimeWindowFactory(List(ParamWrapper(25,Some("min")))).getWindowFunction == SlidingTimeWindow(TimeWindowSize(25,TimeUnit.MINUTES)))
  }

}
