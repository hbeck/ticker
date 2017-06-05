package lars.transform.tuplebased

import core.{Atom, IntValue, NumericArgument, PinnedAtom}
import core.lars.{Box, SlidingTimeWindow, SlidingTupleWindow, WindowAtom}
import lars.transform.TransformLarsSpec
import org.scalatest.Matchers._
import org.scalatest.Inspectors._


/**
  * Created by FM on 05.05.16.
  */
class RuleForBoxSpec extends TransformLarsSpec {
  def rulesForBox(windowAtom: WindowAtom) = allWindowRules(DefaultLarsToPinnedProgram.slidingTuple(windowAtom.windowFunction.asInstanceOf[SlidingTupleWindow], windowAtom))

  def a_TUPLE(arg: NumericArgument) = PinnedAtom.asPinnedAtCntAtom(Atom("a"), T, arg)

  val w_tu_2_b_a = WindowAtom(SlidingTupleWindow(2), Box, a)

  "The rule for w^2 b a" should "contain cnt(C)" in {
    (rulesForBox(w_tu_2_b_a) flatMap (_.body)) should contain(cnt(C))
  }
  it should "generate only one rule" in {
    rulesForBox(w_tu_2_b_a) should have size (3)
  }
  it should "have head w_te_2_b_a(T)" in {
    forAtLeast(1, rulesForBox(w_tu_2_b_a)) { rule => rule.head.toString should include("w_tu_2_b_a") }
  }
  it should "contain a_TUPLE(0)" in {
    (rulesForBox(w_tu_2_b_a) flatMap (_.body)) should contain(a_TUPLE(C))
  }
  it should "contain a_TUPLE(1)" in {
    (rulesForBox(w_tu_2_b_a) flatMap (_.body)) should contain(a_TUPLE(C - 1))
  }
  it should "contain only 3 elements in the body" in {
    rulesForBox(w_tu_2_b_a).head.body should have size (3)
  }

  "The rule for w^3 b a" should "contain a_TUPLE(0) a_TUPLE(1), a_TUPLE(2)" in {
    (rulesForBox(WindowAtom(SlidingTupleWindow(3), Box, a)).
      flatMap(_.body)) should contain.
      allOf(a_TUPLE(C), a_TUPLE(C - 1), a_TUPLE(C - 2))
  }
}
