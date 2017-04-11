package engine.parser.factory

import java.lang.reflect.Constructor

import core.lars.WindowFunction
import engine.parser.InvalidSyntaxException
import engine.parser.factory.slidingWindowFunctionFactory.{SlidingTimeWindowFactory, SlidingTupleWindowFactory}
import engine.parser.wrapper.ParamWrapper

/**
  * Created by et on 22.03.17.
  */
case class ImportFactory(importClass: String, params: Option[String], name: String) {
  ImportFactory.apply(this,importClass,params.getOrElse("").split(",").toList,name)
}

object ImportFactory {

  private var wfnObjects: Map[String,WindowFunctionFactory] = defaultWfnFactories

  def apply(factory: ImportFactory, importClass: String, params: List[String], name: String): Unit = {

    wfnObjects ++= defaultWfnFactories

    val constructor = Class.forName(importClass).getConstructor(params.getClass)

    if(!wfnObjects.contains(name)) {
      wfnObjects += (name -> constructor.newInstance(params).asInstanceOf[WindowFunctionFactory])
    }
  }

  private def defaultWfnFactories: Map[String,WindowFunctionFactory] = {
    Map("t" -> SlidingTimeWindowFactory(List())) + ("#" -> SlidingTupleWindowFactory(List()))
  }

  //TODO throw exception if name is not in wfnobjects
  def getWindowFunctionFactory(name: String): WindowFunctionFactory = {
    val wff = wfnObjects.get(name)

    if(wff.isDefined) return wff.get
    throw new InvalidSyntaxException("The specified window function is invalid")
  }
}
