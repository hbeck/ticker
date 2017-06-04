package engine.evaluation

import core.asp._
import core.lars.{Assignment, TimePoint}
import core.{AtomWithArguments$, GroundAtom, PinnedAtom}
import engine.asp.tms.Pin
import fixtures.TimeTestFixtures
import org.scalatest.FlatSpec
import org.scalatest.Matchers._

/**
  * Created by FM on 16.05.16.
  */
class GroundPinnedAspSpec extends FlatSpec with TimeTestFixtures {

  def Pin(time: TimePoint): Pin = engine.asp.tms.Pin(Assignment(Map(T -> time)))

  "A program containing a(T) :- b(T + 1) at t0" should "be grounded to a(t0) :- b(t1)" in {
    val r = AspRule(a(T), Set(b(T + 1)))

    Pin(t0)(r) should be(AspRule(GroundAtom.fromArguments(a.predicate, t0), Set(GroundAtom.fromArguments(b.predicate, t1))))
  }

  "An atom a(T) at t1" should "be grounded to a(t1)" in {
    Pin(t1)(a(T)) should be(a(t1))
  }
  "An atom a(T + 1) at t1" should "be grounded to a(t2)" in {
    Pin(t1)(a(T + 1)) should be(a(t2))
  }
  "An atom a(t0) at t1" should "be grounded to a(t0)" in {
    Pin(t1)(a(t0)) should be(a(t0))
  }

  "A rule a(T). at t1" should "be grounded to a(t1)." in {
    Pin(t1)(AspFact(a(T))) should be(AspRule(PinnedAtom.asPinnedAtAtom(a, t1)))
  }

  "An atom a(T-1,T) at t1" should "be grounded to a(0,1)" in {
    Pin(t1)(a(T - 1)(T)) should be(a(t0)(t1))
  }
}
