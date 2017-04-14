package engine.parser

import engine.parser.factory.slidingWindowFunctionFactory.{SlidingTimeWindowFactory, SlidingTupleWindowFactory}

/**
  * Created by et on 14.04.17.
  */
object LarsParser extends ParserTrait {

 override def register: Unit = {
    WindowFunctionRegistry.register("t",SlidingTimeWindowFactory())
    WindowFunctionRegistry.register("#",SlidingTupleWindowFactory())
  }
}
