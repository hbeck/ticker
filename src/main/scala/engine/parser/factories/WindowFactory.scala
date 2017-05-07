package engine.parser.factories

import core.lars.WindowFunction
import engine.parser.wrapper.ParamWrapper

/**
  * Created by et on 01.04.17.
  */
case class WindowFactory(w: String, params: List[ParamWrapper] = List()) {

  lazy val wfn: WindowFunction = create(w,params)

  def create(wType: String, params: List[ParamWrapper]): WindowFunction = {
    val wfc = ImportFactory.getWinfowFunction(wType)
    wfc.updateWindowParams(params)
  }


}
