package reasoner.parser.factories.slidingWindowFunctionFactory

import java.util.concurrent.TimeUnit

import core.lars.{TimeWindow, TimeWindowSize, WindowFunction}
import reasoner.parser.InvalidSyntaxException
import reasoner.parser.factories.WindowFunctionFactory
import reasoner.parser.wrapper.ParamWrapper

/**
  * Created by et on 10.04.17.
  */
case class SlidingTimeWindowFactory(params: List[ParamWrapper] = List())
  extends WindowFunctionFactory(params) {

  @throws[InvalidSyntaxException]
  override def create(params: List[ParamWrapper]): WindowFunction = {
    if(params.isEmpty || params.length > 1) throw new InvalidSyntaxException("Sliding time windows can take only one parameter, but "+params.length+" were given.")

    val value = params.head.value.toLong
    if(params.head.unit.isDefined) {
      params.head.unit.get match {
        case "msec" => TimeWindow(TimeWindowSize(value,TimeUnit.MILLISECONDS))
        case "sec" => TimeWindow(TimeWindowSize(value,TimeUnit.SECONDS))
        case "min" => TimeWindow(TimeWindowSize(value,TimeUnit.MINUTES))
        case "h" => TimeWindow(TimeWindowSize(value,TimeUnit.HOURS))
        case other => throw new InvalidSyntaxException("An unknown unit was given: " + other)
      }
    } else {
      TimeWindow(value)
    }
  }

  override def updateWindowParams(params: List[ParamWrapper]): WindowFunction = {
    if(params.nonEmpty) wfn = create(params)
    wfn
  }
}
