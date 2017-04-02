package engine.parser.factory

import core.lars.WindowFunction
import engine.parser.wrapper.ParamWrapper

/**
  * Created by et on 22.03.17.
  */
case class SlidingWindowFactory(w: String, past: Option[ParamWrapper] = None, next: Option[ParamWrapper] = None)
  extends WindowFactory(w) {
  override val wfn: WindowFunction = _
}
