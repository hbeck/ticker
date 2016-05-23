package engine.evaluation

import core.asp.{AspFact, AspProgram, AspRule}
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
    val p = AspProgram.pinned()

    GroundPinnedAsp(t0)(p, Set()).rules should have size 0
  }

  "An empty program with one atom in the datastream" should "contain one rule only" in {
    val p =  AspProgram.pinned()

    GroundPinnedAsp(t0)(p, Set(AspRule(a(T)))).rules should have size 1
  }
  it should "be grounded to t0" in {
    val p =  AspProgram.pinned()

    GroundPinnedAsp(t0)(p, Set(AspRule(a(T)))).atoms should contain only (a(t0))
  }

  "A program containing a(T) :- b(T + 1) at t0" should "be grounded to a(t0) :- b(t1)" in {
    val r = AspRule(a(T), Set(b(T + 1)))

    GroundPinnedAsp(t0)(r) should be(GroundedAspRule(a(t0), Set(b(t1))))
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
    GroundPinnedAsp(t1)(AspRule(a(T))) should be(GroundedAspRule(a(t1)))
  }

  "An atom a(T-1,T) at t1" should "be grounded to a(0,1)" in {
    GroundPinnedAsp(t1)(a(T - 1)(T)) should be(a(t0)(t1))
  }
}
