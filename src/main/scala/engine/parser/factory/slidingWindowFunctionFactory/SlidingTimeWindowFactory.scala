package engine.parser.factory.slidingWindowFunctionFactory

import java.util.concurrent.TimeUnit

import core.lars.{SlidingTimeWindow, TimeWindowSize, WindowFunction}
import engine.parser.factory.WindowFunctionFactory
import engine.parser.wrapper.ParamWrapper

/**
  * Created by et on 10.04.17.
  */
case class SlidingTimeWindowFactory(override val params: List[ParamWrapper]) extends WindowFunctionFactory(params) {


  override protected def create(params: List[ParamWrapper]): WindowFunction = {
    if(params.isEmpty) return SlidingTimeWindow(5*60)

    if(params.length > 1) ??? //TODO throw new InvalidSyntaxException

    val value = params.head.value.toLong
    if(params.head.unit.isDefined) {
      params.head.unit.get match {
        case "micro" => SlidingTimeWindow(TimeWindowSize(value,TimeUnit.MICROSECONDS))
        case "milli" => SlidingTimeWindow(TimeWindowSize(value,TimeUnit.MILLISECONDS))
        case "min" => SlidingTimeWindow(TimeWindowSize(value,TimeUnit.MINUTES))
        case _ => SlidingTimeWindow(TimeWindowSize(value,TimeUnit.SECONDS))
      }
    } else {
      SlidingTimeWindow(value)
    }
  }

  override def updateWindowParams(params: List[ParamWrapper]): Unit = {
    if(params.isEmpty) wfn = create(params)
  }
}
