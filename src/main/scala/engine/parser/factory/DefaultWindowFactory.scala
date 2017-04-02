package engine.parser.factory

import core.lars.WindowFunction
import engine.parser.wrapper.ParamWrapper

/**
  * Created by et on 01.04.17.
  */
case class DefaultWindowFactory(wType: String, past: Option[ParamWrapper], next: Option[ParamWrapper],
                                step: Option[ParamWrapper]) extends WindowFactory(wType) {
  override val wfn: WindowFunction = wfn1(wType,past,next,step)

  private def wfn1(wType: String, past: Option[ParamWrapper], next: Option[ParamWrapper], step: Option[ParamWrapper]):
  WindowFunction = step match {
    case None => wfn2(wType,past,next)
    case _ => ImportFactory.getWindowFunction(wType).get
  }

  private def wfn2(wType: String, past: Option[ParamWrapper], next: Option[ParamWrapper]): WindowFunction = next match {
    case None => wfn3(wType,past)
    case _ => ImportFactory.getWindowFunction(wType).get
  }

  private def wfn3(wType: String, past: Option[ParamWrapper]): WindowFunction = ImportFactory.getWindowFunction(wType).get
}
