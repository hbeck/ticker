package engine.parser.factories

import engine.parser.factories.slidingWindowFunctionFactory.SlidingTimeWindowFactory
import org.scalatest.FlatSpec

/**
  * Created by et on 22.04.17.
  */
class ImportFactoryTest extends FlatSpec {

  behavior of "ImportFactoryTest"

  it should "register" in {
    ImportFactory("engine.parser.factory.slidingWindowFunctionFactory.SlidingTimeWindowFactory",List(),"stw")
    assert(ImportFactory.getWinfowFunction("stw") == SlidingTimeWindowFactory())
  }

  it should "not register a non existent window function factory" in {
    intercept[ImportException] {
      ImportFactory("no.such.WindowFunctionFactory", List(), "fake")
    }
  }
}
