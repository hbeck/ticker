package lars.transform

import core.lars.SlidingTimeWindow
import fixtures.TimeTestFixtures
import org.scalatest.FlatSpec

/**
  * Created by FM on 05.05.16.
  */
class TransformLarsSpec extends FlatSpec with TimeTestFixtures {

  val now = engine.asp.now

  val st1 = SlidingTimeWindow(1)
}
