package lars.transform.timebased

import core.{AtomWithArguments, PinnedAtom}
import core.lars._
import lars.transform.TransformLarsSpec
import org.scalatest.Inspectors._
import org.scalatest.Matchers._

/**
  * Created by FM on 11.05.16.
  */
class RuleForAtSpec extends TransformLarsSpec {
  val w_te_1_a_1_a = WindowAtom(TimeWindow(1), At(t1), a)

  def ruleForAt(windowAtom: WindowAtom) = allWindowRules(DefaultLarsToPinnedProgram.slidingTime(windowAtom.windowFunction.asInstanceOf[TimeWindow], windowAtom))

  "The rule for w^1 at_1 a" should "return two rules" in {
    ruleForAt(w_te_1_a_1_a) should have size (2)
  }
  it should "contain now in all rules" in {
    forAll(ruleForAt(w_te_1_a_1_a)) {
      rule =>
        rule.body collect {
          case a: AtomWithArguments => a.predicate
        } should contain(now)
    }
  }
  it should "contain now(T)" in {
    forAll(ruleForAt(w_te_1_a_1_a)) { rule => rule.body should contain(now(T)) }
  }

  it should "have head w_te_1_at_1_a" in {
    forAll(ruleForAt(w_te_1_a_1_a)) { rule => rule.head.toString should include("w_te_1_at_1_a") }
  }
  // TODO: is the translation for the a rule with At_{t} correct? (when t is a fixed time point)
  it should "contain a(t1) for all elements" in {
    forAll(ruleForAt(w_te_1_a_1_a)) { rule => rule.body should contain(PinnedAtom.asPinnedAtAtom(a, t1)) }
  }

  "The rule for w^2 at_2 a" should "contain now(T), a(t2)" in {
    ruleForAt(WindowAtom(TimeWindow(2), At(t2), a)) flatMap (_.body) should contain allOf(now(T), PinnedAtom.asPinnedAtAtom(a, t2))
  }

  val w_te_1_at_U_a = W(1, At(U), a)

  "The window atom wˆ at_U a" should "have TWOE rule with head w_te_1_at_U_a" in {
    forExactly(2, ruleForAt(w_te_1_at_U_a)) {
      rule => rule.head.toString should startWith("w_te_1_at_U_a")
    }
  }
}
