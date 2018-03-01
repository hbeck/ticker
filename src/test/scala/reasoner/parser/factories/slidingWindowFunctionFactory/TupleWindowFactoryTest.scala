package reasoner.parser.factories.slidingWindowFunctionFactory

import core.lars.TupleWindow
import reasoner.parser.wrapper.ParamWrapper
import org.scalatest.FlatSpec

/**
  * Created by et on 22.04.17.
  */
class TupleWindowFactoryTest extends FlatSpec {

  behavior of "SlidingTupleWindowFactoryTest"

  it should "create" in {
    assert(SlidingTupleWindowFactory(List(ParamWrapper("20","#"))).getWindowFunction == TupleWindow(20))
  }

}
