package lars.transform

import core.lars.{Diamond, SlidingTimeWindow, WindowAtom}
import engine.asp.PlainLarsToAsp
import org.scalatest.Inspectors._
import org.scalatest.Matchers._

/**
  * Created by FM on 09.05.16.
  */
class RuleForDiamondSpec extends TransformLarsSpec {
  val w_1_d_a = WindowAtom(SlidingTimeWindow(1), Diamond, a)

  "The rule for w^1 d a" should "return two rules" in {
    PlainLarsToAsp.rulesForDiamond(w_1_d_a) should have size (2)
  }
  it should "contain now(T) in all rules" in {
    forAll(PlainLarsToAsp.rulesForDiamond(w_1_d_a)) { rule => rule.body should contain(now(T)) }
  }
  it should "have head w_1_b_a" in {
    forAll(PlainLarsToAsp.rulesForDiamond(w_1_d_a)) { rule => rule.head.toString should include("w_1_d_a") }
  }
  it should "contain a(T) for one element" in {
    forExactly(1, PlainLarsToAsp.rulesForDiamond(w_1_d_a)) { rule => rule.body should contain(a(T)) }
  }
  it should "contain a(T - 1)" in {
    forExactly(1, PlainLarsToAsp.rulesForDiamond(w_1_d_a)) { rule => rule.body should contain(a(T - 1)) }
  }

  "The rule for w^3 d a" should "contain a(T -1), a(T -2), a(T -3), a(T)" in {
    PlainLarsToAsp.rulesForDiamond(WindowAtom(SlidingTimeWindow(3), Diamond, a)) flatMap (_.body) should contain allOf(a(T), a(T - 1), a(T - 2), a(T + -3))
  }
}
