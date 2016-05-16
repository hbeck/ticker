package engine.evaluation

import core.asp.{AspFact, AspProgram}
import core.lars.Fact
import engine.asp.evaluation.{AspProgramAtTimePoint, GroundPinnedAsp, PinnedAspRule}
import fixtures.TimeTestFixtures
import org.scalatest.FlatSpec
import org.scalatest.Matchers._

/**
  * Created by FM on 16.05.16.
  */
class GroundPinnedAspSpec extends FlatSpec with TimeTestFixtures {

  "An empty asp-program" should "be grounded to an empty program" in {
    val p = AspProgramAtTimePoint(Seq(), Set(), t0)

    GroundPinnedAsp(p).rules should have size 0
  }

  "A program containing only one pinnedAtom" should "contain one rule only" in {
    val p = AspProgramAtTimePoint(Seq(), Set(PinnedAspRule(a(T),Set())), t0)

    GroundPinnedAsp(p).rules should have size 1
  }
  it should "be grounded to to" in {
    val p = AspProgramAtTimePoint(Seq(), Set(PinnedAspRule(a(T),Set())), t0)

    GroundPinnedAsp(p).atoms should contain only (a(t0))
  }


  "An atom a(T) at t1" should "be grounded to a(t1)" in {
    GroundPinnedAsp(t1)(a(T)) should be(a(t1))
  }
}
