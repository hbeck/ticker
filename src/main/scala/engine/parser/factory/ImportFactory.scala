package engine.parser.factory

import core.lars.WindowFunction
import engine.parser.factory.slidingWindowFunctionFactory.{SlidingTimeWindowFactory, SlidingTupleWindowFactory}
import engine.parser.wrapper.ParamWrapper

/**
  * Created by et on 22.03.17.
  */
case class ImportFactory(importClass: String, params: Option[String], name: String)

object ImportFactory {


  private var wfnObjects: Map[String,WindowFunctionFactory] = Map()

  def apply(importClass: String, paramStr: Option[String], name: String): ImportFactory = {

    wfnObjects ++= defaultWfnFactories

    val params = paramStr.getOrElse("").split(",")
    val constructor = Class.forName(importClass).getConstructor(Class[Array[String]])

    if(!wfnObjects.contains(name)) {
      wfnObjects += (name -> constructor.newInstance(params))
    }
    ImportFactory(importClass, paramStr, name)
  }

  private def defaultWfnFactories: Map[String,WindowFunctionFactory] = {
    Map("t" -> SlidingTimeWindowFactory(List())) + ("#" -> SlidingTupleWindowFactory(List()))
  }

  //TODO throw exception if name is not in wfnobjects
  def getWindowFunctionFactory(name: String): Option[WindowFunctionFactory] = wfnObjects.get(name)
}
