package engine.parser

import engine.parser.factory.WindowFunctionFactory
import engine.parser.factory.slidingWindowFunctionFactory.{SlidingTimeWindowFactory, SlidingTupleWindowFactory}

/**
  * Created by et on 14.04.17.
  */
object WindowFunctionRegistry {

  private var wfnObjects: Map[String,WindowFunctionFactory] = defaultWfnFactories

  def register(name: String, factory: WindowFunctionFactory): Unit = {
    wfnObjects += (name -> factory)
  }

  def getWindowFunctionFactory(name: String, importClass: String = ""): WindowFunctionFactory = {
    var wff = wfnObjects.get(name)

    if(wff.isDefined) return wff.get

    wff = findByFQDN(importClass)
    if(wff.isDefined) return  wff.get

    throw new InvalidSyntaxException("The specified window function is invalid")
  }

  private def findByFQDN(fqdn: String): Option[WindowFunctionFactory] = {
    val wfnf = wfnObjects.find(entry => entry._2.getClass.toString.split(" ")(1) == fqdn)
    if (wfnf.isDefined) return Some(wfnf.get._2)
    None
  }

  private def defaultWfnFactories: Map[String,WindowFunctionFactory] = {
    Map("t" -> SlidingTimeWindowFactory(List())) + ("#" -> SlidingTupleWindowFactory(List()))
  }
}
