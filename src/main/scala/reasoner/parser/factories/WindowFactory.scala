package reasoner.parser.factories

import core.lars.WindowFunction
import reasoner.parser.InvalidSyntaxException
import reasoner.parser.factories.slidingWindowFunctionFactory.{SlidingTimeWindowFactory, SlidingTupleWindowFactory}
import reasoner.parser.wrapper.ParamWrapper

/**
  * Created by et on 01.04.17.
  */
case class WindowFactory(w: String, params: List[ParamWrapper] = List()) {

  lazy val wfn: WindowFunction = create(w,params)

  def create(wType: String, params: List[ParamWrapper]): WindowFunction = wType match {
    case "t" => SlidingTimeWindowFactory(params).getWindowFunction
    case "#" => SlidingTupleWindowFactory(params).getWindowFunction
    case str => throw new InvalidSyntaxException(str+" is not a valid window type.")
  }
}
