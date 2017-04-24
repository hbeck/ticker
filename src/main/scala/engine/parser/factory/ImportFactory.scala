package engine.parser.factory

import engine.parser.InvalidSyntaxException
import engine.parser.factory.slidingWindowFunctionFactory._
import engine.parser.wrapper.ParamWrapper


/**
  * Created by et on 22.03.17.
  */

case class ImportFactory(importClass: String, params: List[ParamWrapper], name: String) {
  ImportFactory.register(importClass,params,name)
}

object ImportFactory {

  private var importFactories: Map[String,WindowFunctionFactory] = defaultFactories

  def register(importClass: String, params: List[ParamWrapper], name: String): Unit = {

    val clazz = Class.forName(importClass)
    val constructor = clazz.getConstructor(classOf[List[WindowFunctionFactory]])
    
    constructor.newInstance(params) match {
      case factory:WindowFunctionFactory => importFactories += (name -> factory)
      case _ => throw new ImportException("The specified class is not a subtype of WindowFunctionFactory")
    }
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
