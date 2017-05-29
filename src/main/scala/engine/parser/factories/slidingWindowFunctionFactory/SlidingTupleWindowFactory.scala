package engine.parser.factories.slidingWindowFunctionFactory

import core.lars.{SlidingTupleWindow, WindowFunction}
import engine.parser.InvalidSyntaxException
import engine.parser.factories.WindowFunctionFactory
import engine.parser.wrapper.ParamWrapper

/**
  * Created by et on 10.04.17.
  */
case class SlidingTupleWindowFactory(params: List[ParamWrapper] = List())
  extends WindowFunctionFactory(params) {

  @throws[InvalidSyntaxException]
  override def create(params: List[ParamWrapper]): WindowFunction = {
    if(params.isEmpty) return SlidingTupleWindow(5)
    else if (params.length > 1) throw new InvalidSyntaxException("Sliding time windows can take only one parameter, but "+params.length+" were given.")

    val size = params.head.value.toLong
    if(size < 1) throw new InvalidSyntaxException("The window size for tuple based windows cannot be smaller than one tuple.")

    SlidingTupleWindow(size)
  }

  override def updateWindowParams(params: List[ParamWrapper]): WindowFunction = {
  if(params.nonEmpty) wfn = create(params)
    wfn
  }
}
