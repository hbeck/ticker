package engine.parser.factory

import core.lars.WindowFunction
import engine.parser.WindowFunctionRegistry
import engine.parser.wrapper.ParamWrapper

/**
  * Created by et on 22.03.17.
  */
case class ImportFactory(importClass: String, params: List[ParamWrapper], name: String) {
  val wfnf: WindowFunction = create(importClass,params,name)

  private def create(importClass: String, params: List[ParamWrapper], name: String): WindowFunction = {
    WindowFunctionRegistry.getWindowFunctionFactory(name,importClass).create(params)
  }

/*  private def convertToParamWrapper(params: List[String]): List[Option[ParamWrapper]] = params match {
    case Nil => Nil
    case x::xs => splitString(x) +: convertToParamWrapper(xs)
  }

  private def splitString(x: String): Option[ParamWrapper] = {
    val split = x.split(" ")
    if(split.length == 2) {
      Some(ParamWrapper(split(0),split(1)))
    } else if(split.length == 1 && split(0).nonEmpty) {
      Some(ParamWrapper(split(0)))
    } else if(split.length > 2) throw new InvalidSyntaxException("The unit of a parameter cannot contain spaces.")
    else None
  }*/
}
