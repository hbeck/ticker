package engine.parser.factory

import core.lars.{SlidingTimeWindow, WindowFunction}
import engine.parser.wrapper.ParamWrapper

/**
  * Created by et on 01.04.17.
  */
case class WindowFactory(w: String, params: List[ParamWrapper]) {


  val wfn: WindowFunction = create(w,params)


  def create(wType: String, params: List[ParamWrapper]): WindowFunction = {
    val wfc = ImportFactory.getWindowFunctionFactory(wType)
    wfc.updateWindowParams(params)
  }


}
