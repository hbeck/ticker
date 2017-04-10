package engine.parser.factory.slidingWindowFunctionFactory

import core.lars.{SlidingTupleWindow, WindowFunction}
import engine.parser.factory.WindowFunctionFactory
import engine.parser.wrapper.ParamWrapper

/**
  * Created by et on 10.04.17.
  */
case class SlidingTupleWindowFactory(override val params: List[ParamWrapper]) extends WindowFunctionFactory(params) {

  override protected def create(params: List[ParamWrapper]): WindowFunction = {
    if(params.isEmpty) return SlidingTupleWindow(5)
    else if (params.length > 1) ??? //TODO throw new InvalidSyntaxException

    SlidingTupleWindow(params.head.value.toLong)
  }

  override def updateWindowParams(params: List[ParamWrapper]): WindowFunction = {
  if(params.nonEmpty) wfn = create(params)
    wfn
  }

/*  private def checkParams(params: List[ParamWrapper]): Boolean = {
    params.length <= 1 && (params(0) forall Character.isDigit)
  }*/

}
