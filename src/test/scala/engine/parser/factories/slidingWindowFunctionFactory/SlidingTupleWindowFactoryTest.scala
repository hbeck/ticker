package engine.parser.factories.slidingWindowFunctionFactory

import core.lars.SlidingTupleWindow
import engine.parser.wrapper.ParamWrapper
import org.scalatest.FlatSpec

/**
  * Created by et on 22.04.17.
  */
class SlidingTupleWindowFactoryTest extends FlatSpec {

  behavior of "SlidingTupleWindowFactoryTest"

  it should "create" in {
    assert(SlidingTupleWindowFactory(List(ParamWrapper("20","#"))).getWindowFunction == SlidingTupleWindow(20))
  }

}
