package lars.transform

import core.lars.{Box, SlidingTimeWindow, WindowAtom}
import engine.TransformLars
import org.scalatest.Matchers._
import org.scalatest.OptionValues._

/**
  * Created by FM on 05.05.16.
  */
class RuleForBoxSpec extends TransformLarsSpec {

  val w_1_b_a = WindowAtom(SlidingTimeWindow(1), Box, a)
  "The rule for w^1 b a" should "contain now(T)" in {
    TransformLars.ruleForBox(w_1_b_a).body should contain(now(T))
  }
  it should "have head w_1_b_a" in {
    TransformLars.ruleForBox(w_1_b_a).head.toString should be("w_1_b_a")
  }
  it should "contain a(T)" in {
    TransformLars.ruleForBox(w_1_b_a).body should contain(a(T))
  }
  it should "contain a(T - 1)" in {
    TransformLars.ruleForBox(w_1_b_a).body should contain(a(T + "-1"))
  }

  "The rule for w^3 b a" should "contain a(T -1), a(T -2), a(T -3)" in {
    TransformLars.ruleForBox(WindowAtom(SlidingTimeWindow(3), Box, a)).body should contain allOf(a(T + "-1"), a(T + "-2"), a(T + "-3"))
  }
}
