package engine.parser.factories

import core.lars.{SlidingTimeWindow, TimeWindowSize}
import engine.parser.InvalidSyntaxException
import engine.parser.wrapper.ParamWrapper
import org.scalatest.FlatSpec

/**
  * Created by et on 22.04.17.
  */
class WindowFactoryTest extends FlatSpec {

  behavior of "WindowFactoryTest"

  it should "create" in {
    assert(WindowFactory("t",List()).wfn == SlidingTimeWindow(TimeWindowSize(300)))
    assert(WindowFactory("t",List(ParamWrapper("10"))).wfn == SlidingTimeWindow(TimeWindowSize(10)))
  }

  it should "reject non existent window types" in {
    intercept[InvalidSyntaxException] {
      WindowFactory("wrong",List()).wfn
    }
  }
}
