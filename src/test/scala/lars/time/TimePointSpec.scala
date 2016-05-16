package lars.time

import core.lars.TimePoint
import org.scalatest.FlatSpec
import org.scalatest.Matchers._

/**
  * Created by FM on 16.05.16.
  */
class TimePointSpec extends FlatSpec {

  val t0 = TimePoint(0)
  val t1 = TimePoint(1)
  val t2 = TimePoint(2)

  "A TimePoint t1" should "be converted to 't1'" in {
    t1.toString should be("1")
  }

  "Adding 1 to TimePoint t1" should "lead to t2" in {
    (t1 + 1) should be(t2)
  }
  "Subtracting 1 from TimePoint t1" should "lead to t0" in {
    (t1 - 1) should be(t0)
  }

}
