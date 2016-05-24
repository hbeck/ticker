package fixtures

import core.lars.{TimePoint, TimeVariableWithOffset}

/**
  * Created by FM on 16.05.16.
  */
trait TimeTestFixtures extends AtomTestFixture {
  val t0 = TimePoint(0)
  val t1 = TimePoint(1)
  val t2 = TimePoint(2)
  val t3 = TimePoint(3)
  val t4 = TimePoint(4)

  val U = TimeVariableWithOffset("U")
  val T =  core.lars.T
}
