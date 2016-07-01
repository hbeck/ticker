package engine.evaluation

import core.NonGroundAtom
import engine.asp._
import engine.asp.oneshot.StreamingClingoInterpreter
import fixtures.TimeTestFixtures
import org.scalatest.FlatSpec
import org.scalatest.Matchers._

/**
  * Created by FM on 22.04.16.
  */
class StreamingAspToClingoSpec extends FlatSpec with TimeTestFixtures {

  "An empty set of ASP-Expressions" should "return an empty result" in {
    val convert = StreamingClingoInterpreter(Set())
    convert(t0, Set()) should be(empty)
  }

  "An atom 'a(bar, 1)'" should "be converted to result a(bar, t1)" in {
    StreamingClingoInterpreter.asPinnedAtom(Set(a("bar", "1")), t2) should contain only (a("bar")(t1))
  }

  "An atom 'a(0)'" should "be converted to a(t0)" in {
    StreamingClingoInterpreter.asPinnedAtom(Set(NonGroundAtom(a, Seq(t0.toString))), t1) should contain(a(t0))
  }

  "A model containing a(1), now(0) and a(2)" should "be converted into a(t1), now(t0), a(t2) at t0" in {
    val modelAfterClingoParsing = Set(a("1"), now("0"), a("2"))
    StreamingClingoInterpreter.asPinnedAtom(modelAfterClingoParsing.toSet, t0) should contain allOf(a(t1), now(t0), a(t2))
  }
}
