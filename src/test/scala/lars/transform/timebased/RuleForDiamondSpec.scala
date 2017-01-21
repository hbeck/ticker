package lars.transform.timebased

import core.lars.{Diamond, SlidingTimeWindow, WindowAtom}
import engine.asp.LarsToPinnedProgram
import lars.transform.TransformLarsSpec
import org.scalatest.Inspectors._
import org.scalatest.Matchers._

import scala.concurrent.duration._

/**
  * Created by FM on 09.05.16.
  */
class RuleForDiamondSpec extends TransformLarsSpec {
  val w_te_1_d_a = WindowAtom(SlidingTimeWindow(1), Diamond, a)

  "The rule for w^1 d a" should "return two rules" in {
    DefaultLarsToPinnedProgram.bodiesForDiamond(w_te_1_d_a) should have size (2)
  }
  it should "contain now(T) in all rules" in {
    forAll(DefaultLarsToPinnedProgram.bodiesForDiamond(w_te_1_d_a)) { rule => rule.body should contain(now(T)) }
  }
  it should "have head w_1_b_a" in {
    forAll(DefaultLarsToPinnedProgram.bodiesForDiamond(w_te_1_d_a)) { rule => rule.head.toString should include("w_te_1_d_a") }
  }
  it should "contain a(T) for one element" in {
    forExactly(1, DefaultLarsToPinnedProgram.bodiesForDiamond(w_te_1_d_a)) { rule => rule.body should contain(a(T)) }
  }
  it should "contain a(T - 1)" in {
    forExactly(1, DefaultLarsToPinnedProgram.bodiesForDiamond(w_te_1_d_a)) { rule => rule.body should contain(a(T - 1)) }
  }

  "The rule for w^3 d a" should "contain a(T -1), a(T -2), a(T -3), a(T)" in {
    DefaultLarsToPinnedProgram.bodiesForDiamond(WindowAtom(SlidingTimeWindow(3), Diamond, a)) flatMap (_.body) should contain allOf(a(T), a(T - 1), a(T - 2), a(T + -3))
  }

  "The rule for w^1s d a at tick size of 1ms" should "return 1001 rules" in {
    LarsToPinnedProgram(1 millisecond).bodiesForDiamond(w_te_1_d_a) should have size (1001)
  }
}
