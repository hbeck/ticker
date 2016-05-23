package lars.time

import fixtures.TimeTestFixtures
import org.scalatest.FlatSpec
import org.scalatest.Matchers._

/**
  * Created by FM on 16.05.16.
  */
class TimePointSpec extends FlatSpec with TimeTestFixtures{

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
