package engine.parser.factory

import core.lars.WindowFunction

/**
  * Created by et on 22.03.17.
  */
case class WindowFactory(wType: String, past: Option[ParamFactory] = None, next: Option[ParamFactory] = None,
                         step: Option[ParamFactory] = None)  {

  val wfn: WindowFunction = wfn1(wType,past,next,step)

  private def wfn1(wType: String, past: Option[ParamFactory], next: Option[ParamFactory], step: Option[ParamFactory]):
    WindowFunction = step match {
    case None => wfn2(wType,past,next)
    case s => ImportFactory.getWfnObject(wType,past,next,s)
  }

  private def wfn2(wType: String, past: Option[ParamFactory], next: Option[ParamFactory]): WindowFunction = next match {
    case None => wfn3(wType,past)
    case n => ImportFactory.getWfnObject(wType,past,n)
  }

  private def wfn3(wType: String, past: Option[ParamFactory]): WindowFunction = past match {
    case None => ImportFactory.getWfnObject(wType)
    case p => ImportFactory.getWfnObject(wType,p)
  }

// if(windowFunction.isDefined) {
//  val foo = windowFunction.get
// }
}
