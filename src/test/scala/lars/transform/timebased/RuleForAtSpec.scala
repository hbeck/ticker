package lars.transform.timebased

import core.AtomWithArgument
import core.lars._
import lars.transform.TransformLarsSpec
import org.scalatest.Inspectors._
import org.scalatest.Matchers._

/**
  * Created by FM on 11.05.16.
  */
class RuleForAtSpec extends TransformLarsSpec {
  val w_te_1_a_1_a = WindowAtom(SlidingTimeWindow(1), At(t1), a)

  def ruleForAt(windowAtom: WindowAtom) = allWindowRules(DefaultLarsToPinnedProgram.slidingTime(windowAtom.windowFunction.asInstanceOf[SlidingTimeWindow], windowAtom))

  "The rule for w^1 at_1 a" should "return two rules" in {
    ruleForAt(w_te_1_a_1_a) should have size (2)
  }
  it should "contain now in all rules" in {
    forAll(ruleForAt(w_te_1_a_1_a)) {
      rule =>
        rule.body collect {
          case a: AtomWithArgument => a.predicate
        } should contain(now.predicate)
    }
  }
  it should "contain now(t2)" in {
    forExactly(1, ruleForAt(w_te_1_a_1_a)) { rule => rule.body should contain(now(t2)) }
  }
  it should "contain now(t1)" in {
    forExactly(1, ruleForAt(w_te_1_a_1_a)) { rule => rule.body should contain(now(t1)) }
  }
  it should "have head w_te_1_at_1_a" in {
    forAll(ruleForAt(w_te_1_a_1_a)) { rule => rule.head.toString should include("w_te_1_at_1_a") }
  }
  it should "contain a(t1) for all elements" in {
    forAll(ruleForAt(w_te_1_a_1_a)) { rule => rule.body should contain(a(t1)) }
  }

  "The rule for w^2 at_2 a" should "contain now(t3), now(t4), now(t2), a(t2)" in {
    ruleForAt(WindowAtom(SlidingTimeWindow(2), At(t2), a)) flatMap (_.body) should contain allOf(now(t3), now(t4), now(t2), a(t2))
  }

  val w_te_1_at_U_a = W(1, At(U), a)

  "The window atom wˆ at_U a" should "have TWOE rule with head w_te_1_at_U_a" in {
    forExactly(2, ruleForAt(w_te_1_at_U_a)) {
      rule => rule.head.toString should startWith("w_te_1_at_U_a")
    }
  }
}
