package engine.parser.wrapper

import core.lars.WindowFunction

/**
  * Created by et on 22.03.17.
  */
case class WindowWrapper(wType: String, past: Option[ParamWrapper] = None, next: Option[ParamWrapper] = None,
                         step: Option[ParamWrapper] = None)  {

  val wfn: WindowFunction = wfn1(wType,past,next,step)

  private def wfn1(wType: String, past: Option[ParamWrapper], next: Option[ParamWrapper], step: Option[ParamWrapper]):
    WindowFunction = step match {
    case None => wfn2(wType,past,next)
    case s => ImportWrapper.getWfnObject(wType,past,next,s)
  }

  private def wfn2(wType: String, past: Option[ParamWrapper], next: Option[ParamWrapper]): WindowFunction = next match {
    case None => wfn3(wType,past)
    case n => ImportWrapper.getWfnObject(wType,past,n)
  }

  private def wfn3(wType: String, past: Option[ParamWrapper]): WindowFunction = past match {
    case None => ImportWrapper.getWfnObject(wType)
    case p => ImportWrapper.getWfnObject(wType,p)
  }

// if(windowFunction.isDefined) {
//  val foo = windowFunction.get
// }
}
