package engine.parser.factory

import core.lars.{SlidingTupleWindow, WindowFunction}
import engine.parser.InvalidSyntaxException
import engine.parser.factory.slidingWindowFunctionFactory.{SlidingTimeWindowFactory, SlidingTupleWindowFactory}
import engine.parser.utils.WindowFunctionRegistry
import engine.parser.wrapper.ParamWrapper


/**
  * Created by et on 22.03.17.
  */
//case class ImportFactory(importClass: String, params: List[ParamWrapper], name: String) {
//  val wfnf: WindowFunctionFactory = ImportFactory.apply(importClass,params,name)

//  private def create(importClass: String, params: List[ParamWrapper], name: String): WindowFunction = {
////    WindowFunctionRegistry.getFactory(importClass).create(params)
//    ImportFactory.register(importClass,params,name)
//  }
//}

case class ImportFactory(importClass: String, params: List[ParamWrapper], name: String) {
  ImportFactory.register(importClass,params,name)
}

object ImportFactory {

  private var importFactories: Map[String,WindowFunctionFactory] = defaultFactories

  def register(importClass: String, params: List[ParamWrapper], name: String): Unit = {
    val factory = WindowFunctionRegistry.getFactory(importClass)
    importFactories += (name -> factory)
  }

  def getWinfowFunction(name: String): WindowFunctionFactory = {
    val wfn = importFactories.get(name)
    if(wfn.isDefined) return wfn.get
    throw new InvalidSyntaxException("The specified window function is invalid")
  }

  private def defaultFactories: Map[String,WindowFunctionFactory] = {
    Map("t" -> SlidingTimeWindowFactory(),
        "#" -> SlidingTupleWindowFactory())
  }
}
