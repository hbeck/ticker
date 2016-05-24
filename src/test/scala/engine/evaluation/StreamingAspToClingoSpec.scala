package engine.evaluation

import core.AtomWithArguments
import engine.asp._
import engine.asp.evaluation.{AspEvaluationEngine, StreamingClingoInterpreter}
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

  "An atom 'a'" should "be converted to result a(t1)" in {
    StreamingClingoInterpreter.asPinnedAtom(Set(a), t1) should contain only (a(t1))
  }
  "An atom 'a(bar)'" should "be converted to result a(bar, t1)" in {
    StreamingClingoInterpreter.asPinnedAtom(Set(a("bar")), t1) should contain only (a("bar")(t1))
  }

  "An atom 'a(0)'" should "be converted to a(t0)" in {
    StreamingClingoInterpreter.asPinnedAtom(Set(AtomWithArguments(a, Seq(t0.toString))), t1) should contain(a(t0))
  }

  "A model containing a(1), now(0) and a(2)" should "be converted into a(t1), now(t0), a(t2) at t0" in {
    val modelAfterClingoParsing = Set(a("1"), now("0"), a("2"))
    StreamingClingoInterpreter.asPinnedAtom(modelAfterClingoParsing, t0) should contain allOf(a(t1), now(t0), a(t2))
  }
}
