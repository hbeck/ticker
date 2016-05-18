package engine.evaluation

import core.asp.{AspFact, AspProgram}
import core.lars.Fact
import engine.asp.evaluation.{GroundPinnedAsp, GroundedAspRule, PinnedAspProgram, PinnedAspRule}
import fixtures.TimeTestFixtures
import org.scalatest.FlatSpec
import org.scalatest.Matchers._

/**
  * Created by FM on 16.05.16.
  */
class GroundPinnedAspSpec extends FlatSpec with TimeTestFixtures {

  "An empty asp-program" should "be grounded to an empty program" in {
    val p = PinnedAspProgram(Seq())

    GroundPinnedAsp(t0)(p, Set()).rules should have size 0
  }

  "A program containing only one pinnedAtom" should "contain one rule only" in {
    val p = PinnedAspProgram(Seq())

    GroundPinnedAsp(t0)(p, Set(PinnedAspRule(a(T), Set()))).rules should have size 1
  }
  it should "be grounded to t0" in {
    val p = PinnedAspProgram(Seq())

    GroundPinnedAsp(t0)(p, Set(PinnedAspRule(a(T), Set()))).atoms should contain only (a(t0))
  }

  "An atom a(T) at t1" should "be grounded to a(t1)" in {
    GroundPinnedAsp(t1)(a(T)) should be(a(t1))
  }
  "An atom a(T + 1) at t1" should "be grounded to a(t2)" in {
    GroundPinnedAsp(t1)(a(T + 1)) should be(a(t2))
  }
  "An atom a(t0) at t1" should "be grounded to a(t0)" in {
    GroundPinnedAsp(t1)(a(t0)) should be(a(t0))
  }

  "A rule a(T). at t1" should "be grounded to a(t1)." in {
    GroundPinnedAsp(t1)(PinnedAspRule(a(T), Set())) should be(GroundedAspRule(a(t1)))
  }
}
