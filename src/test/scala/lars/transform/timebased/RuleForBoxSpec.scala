package lars.transform.timebased

import core.lars.{Box, SlidingTimeWindow, WindowAtom}
import engine.asp.PlainLarsToAsp
import lars.transform.TransformLarsSpec
import org.scalatest.Matchers._


/**
  * Created by FM on 05.05.16.
  */
class RuleForBoxSpec extends TransformLarsSpec {

  val w_1_b_a = WindowAtom(SlidingTimeWindow(1), Box, a)

  "The rule for w^1 b a" should "contain now(T)" in {
    (PlainLarsToAsp.rulesForBox(w_1_b_a) flatMap (_.body)) should contain(now(T))
  }
  it should "have head w_1_b_a(T)" in {
    PlainLarsToAsp.rulesForBox(w_1_b_a).head.head.toString should include("w_1_b_a")
  }
  it should "contain a(T)" in {
    (PlainLarsToAsp.rulesForBox(w_1_b_a) flatMap (_.body)) should contain(a(T))
  }
  it should "contain a(T - 1)" in {
    (PlainLarsToAsp.rulesForBox(w_1_b_a) flatMap (_.body)) should contain(a(T - 1))
  }

  "The rule for w^3 b a" should "contain a(T) a(T -1), a(T -2), a(T -3)" in {
    (PlainLarsToAsp.rulesForBox(WindowAtom(SlidingTimeWindow(3), Box, a)) flatMap (_.body)) should contain allOf(a(T), a(T - 1), a(T - 2), a(T - 3))
  }
}
