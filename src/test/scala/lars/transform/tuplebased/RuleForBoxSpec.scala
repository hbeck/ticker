package lars.transform.tuplebased

import core.{Atom, IntValue}
import core.lars.{Box, SlidingTimeWindow, SlidingTupleWindow, WindowAtom}
import lars.transform.TransformLarsSpec
import org.scalatest.Matchers._


/**
  * Created by FM on 05.05.16.
  */
class RuleForBoxSpec extends TransformLarsSpec {
  def rulesForBox(windowAtom: WindowAtom)=DefaultLarsToPinnedProgram.slidingTuple(windowAtom.windowFunction.asInstanceOf[SlidingTupleWindow],windowAtom).allWindowRules

  def a_TUPLE(pos:Int) = Atom("a").asTupleReference(pos)

  val w_tu_2_b_a = WindowAtom(SlidingTupleWindow(2), Box, a)

  "The rule for w^2 b a" should "contain now(T)" in {
    (rulesForBox(w_tu_2_b_a) flatMap (_.body)) should contain(now(T))
  }
  it should "generate only one rule" in{
    rulesForBox(w_tu_2_b_a) should have size(1)
  }
  it should "have head w_te_2_b_a(T)" in {
    rulesForBox(w_tu_2_b_a).head.head.toString should include("w_tu_2_b_a")
  }
  it should "contain a_TUPLE(0)" in {
    (rulesForBox(w_tu_2_b_a) flatMap (_.body)) should contain(a_TUPLE(0))
  }
  it should "contain a_TUPLE(1)" in {
    (rulesForBox(w_tu_2_b_a) flatMap (_.body)) should contain(a_TUPLE(1))
  }
  it should "contain only 3 elements in the body" in {
    rulesForBox(w_tu_2_b_a).head.body should have size(3)
  }

  "The rule for w^3 b a" should "contain a_TUPLE(0) a_TUPLE(1), a_TUPLE(2)" in {
    (rulesForBox(WindowAtom(SlidingTupleWindow(3), Box, a)).
      flatMap(_.body)) should contain.
      allOf(a_TUPLE(0), a_TUPLE(1), a_TUPLE(2))
  }
}
