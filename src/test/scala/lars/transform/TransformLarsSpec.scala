package lars.transform

import core.lars.SlidingTimeWindow
import engine.asp.{ PlainLarsToAspMapper}
import fixtures.TimeTestFixtures
import org.scalatest.FlatSpec

import scala.concurrent.duration._

/**
  * Created by FM on 05.05.16.
  */
class TransformLarsSpec extends FlatSpec with TimeTestFixtures {

  val now = engine.asp.now

  val st1 = SlidingTimeWindow(1)

  def DefaultLarsToPinnedProgram: PlainLarsToAspMapper = engine.asp.PlainLarsToAspMapper(1 second)

}
