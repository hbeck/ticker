package lars.transform.tuplebased

import core.Atom
import core.lars.{Box, SlidingTimeWindow, SlidingTupleWindow, WindowAtom}
import engine.asp.PlainLarsToAsp
import lars.transform.TransformLarsSpec
import org.scalatest.Matchers._


/**
  * Created by FM on 05.05.16.
  */
class RuleForBoxSpec extends TransformLarsSpec {
  val a_TUPLE = Atom("a_TUPLE")

  val w_tu_1_b_a = WindowAtom(SlidingTupleWindow(1), Box, a)

  "The rule for w^1 b a" should "contain now(T)" in {
    (PlainLarsToAsp.rulesForBox(w_tu_1_b_a) flatMap (_.body)) should contain(now(T))
  }
  it should "have head w_te_1_b_a(T)" in {
    PlainLarsToAsp.rulesForBox(w_tu_1_b_a).head.head.toString should include("w_tu_1_b_a")
  }
  it should "contain a_TUPLE(0)" in {
    (PlainLarsToAsp.rulesForBox(w_tu_1_b_a) flatMap (_.body)) should contain(a_TUPLE(0))
  }
  it should "contain a_TUPLE(1)" in {
    (PlainLarsToAsp.rulesForBox(w_tu_1_b_a) flatMap (_.body)) should contain(a_TUPLE(1))
  }

  "The rule for w^3 b a" should "contain a_TUPLE(0) a_TUPLE(1), a_TUPLE(2), a_TUPLE(3)" in {
    (PlainLarsToAsp.rulesForBox(WindowAtom(SlidingTupleWindow(3), Box, a)).
      flatMap(_.body)) should contain.
      allOf(a_TUPLE(0), a_TUPLE(1), a_TUPLE(2), a_TUPLE(3))
  }
}
