package lars.time

import core.{IntValue, Value}
import fixtures.TimeTestFixtures
import org.scalatest.FlatSpec
import org.scalatest.Matchers._

/**
  * Created by FM on 19.05.16.
  */
class AtomWithTimeSpec extends FlatSpec with TimeTestFixtures {
  "An atom a with time t1 " should "have one argument T" in {
    a(T).arguments should contain only (T.variable)
  }
  "An atom a(1) with time t1" should "have arguments 1, T" in {
    a("1")(T).arguments should contain inOrderOnly(Value("1"), T.variable)
  }
  "An atom with TimeVariable U at time t1" should "have arguments U, T" in {
    val atom = a(U)
    atom(T).arguments should contain inOrderOnly(U.variable, T.variable)
  }
}
