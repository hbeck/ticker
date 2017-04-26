package engine.parser.utils

import engine.parser.InvalidSyntaxException
import engine.parser.factory.WindowFunctionFactory
import engine.parser.factory.slidingWindowFunctionFactory.{SlidingTimeWindowFactory, SlidingTupleWindowFactory}

/**
  * Created by et on 14.04.17.
  */
@deprecated
object WindowFunctionRegistry {

  private var factories: Map[String,WindowFunctionFactory] = defaultWfnFactories

  def register(factory: WindowFunctionFactory): Unit = {
    factories += (getFQDN(factory) -> factory)
  }

  @throws[InvalidSyntaxException]
  def getFactory(importClass: String): WindowFunctionFactory = {
    val wff = factories.get(importClass)

    if(wff.isDefined) return wff.get
    throw new InvalidSyntaxException("The specified window function is invalid")
  }

  private def defaultWfnFactories: Map[String,WindowFunctionFactory] = {
    val time = SlidingTimeWindowFactory()
    val tuple = SlidingTupleWindowFactory()
    Map(getFQDN(time) -> time) + (getFQDN(tuple) -> tuple)
  }

  private def getFQDN(factory: WindowFunctionFactory): String = {
    factory.getClass.toString.split(" ")(1)
  }
}
