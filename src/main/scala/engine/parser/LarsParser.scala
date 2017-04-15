package engine.parser

import engine.parser.factory.slidingWindowFunctionFactory.{SlidingTimeWindowFactory, SlidingTupleWindowFactory}
import engine.parser.utils.WindowFunctionRegistry

/**
  * Created by et on 14.04.17.
  */
object LarsParser extends ParserTrait {

 override def register(): Unit = {
    WindowFunctionRegistry.register(SlidingTimeWindowFactory())
    WindowFunctionRegistry.register(SlidingTupleWindowFactory())
  }
}
