package reasoner.evaluation

import clingo.{ClingoProgramWithLars, PlainClingoProgram}
import core.{NonGroundAtom, PinnedAtom}
import reasoner.asp._
import reasoner.asp.oneshot.StreamingClingoInterpreter
import fixtures.TimeTestFixtures
import org.scalatest.FlatSpec
import org.scalatest.Matchers._

/**
  * Created by FM on 22.04.16.
  */
class StreamingAspToClingoSpec extends FlatSpec with TimeTestFixtures {

  "An empty set of ASP-Expressions" should "return an empty result" in {
    val convert = StreamingClingoInterpreter(ClingoProgramWithLars(Set(), Seq(), 0))
    convert(t0, Set()) should be(empty)
  }

  "An atom 'a(bar, 1)'" should "be converted to result a(bar, t1)" in {
    StreamingClingoInterpreter.asPinnedAtom(Set(a("bar", "1")), t2) should contain only (PinnedAtom.asPinnedAtAtom(a("bar"), t1))
  }

  "An atom 'a(0)'" should "be converted to a(t0)" in {
    StreamingClingoInterpreter.asPinnedAtom(Set(NonGroundAtom(a.predicate, Seq(t0.toString))), t1) should contain(PinnedAtom.asPinnedAtAtom(a,t0))
  }

  "A model containing a(1), now(0) and a(2)" should "be converted into a(t1), now(t0), a(t2) at t0" in {
    val modelAfterClingoParsing = Set(a("1"), now("0"), a("2"))
    StreamingClingoInterpreter.asPinnedAtom(modelAfterClingoParsing, t0) should contain allOf(now(t0), PinnedAtom.asPinnedAtAtom(a, t1), PinnedAtom.asPinnedAtAtom(a, t2))
  }
}
