package engine.parser.factories

import core.lars.WindowFunction
import engine.parser.wrapper.ParamWrapper

/**
  * Created by et on 10.04.17.
  */
abstract class WindowFunctionFactory(params: List[ParamWrapper]) {

  protected var wfn: WindowFunction = create(params)

  def create(params: List[ParamWrapper]): WindowFunction
  def updateWindowParams(params: List[ParamWrapper]): WindowFunction
  def getWindowFunction: WindowFunction = wfn
}
