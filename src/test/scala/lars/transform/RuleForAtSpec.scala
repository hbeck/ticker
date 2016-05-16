package lars.transform

import core.{Atom, AtomWithArgument, AtomWithArguments}
import core.lars._
import engine.asp.PlainLarsToAsp
import org.scalatest.Inspectors._
import org.scalatest.Matchers._

/**
  * Created by FM on 11.05.16.
  */
class RuleForAtSpec extends TransformLarsSpec {
  val w_1_a_1_a = WindowAtom(SlidingTimeWindow(1), At(t1), a)

  "The rule for w^1 at_1 a" should "return two rules" in {
    PlainLarsToAsp.rulesForAt(w_1_a_1_a) should have size (2)
  }
  it should "contain now in all rules" in {
    forAll(PlainLarsToAsp.rulesForAt(w_1_a_1_a)) {
      rule => rule.body map {
        case a: AtomWithArgument => a.atom
      } should contain(now)
    }
  }
  it should "contain now(t0)" in {
    forExactly(1, PlainLarsToAsp.rulesForAt(w_1_a_1_a)) { rule => rule.body should contain(now(t0)) }
  }
  it should "contain now(t1)" in {
    forExactly(1, PlainLarsToAsp.rulesForAt(w_1_a_1_a)) { rule => rule.body should contain(now(t1)) }
  }
  it should "have head w_1_at_1_a" in {
    forAll(PlainLarsToAsp.rulesForAt(w_1_a_1_a)) { rule => rule.head.toString should include("w_1_at_1_a") }
  }
  it should "contain a(t1) for all elements" in {
    forAll(PlainLarsToAsp.rulesForAt(w_1_a_1_a)) { rule => rule.body should contain(a(t1)) }
  }

  "The rule for w^2 at_2 a" should "contain now(t0), now(t1), now(t2), a(t2)" in {
    PlainLarsToAsp.rulesForAt(WindowAtom(SlidingTimeWindow(2), At(t2), a)) flatMap (_.body) should contain allOf(now(t0), now(t1), now(t2), a(t2))
  }

  val w_1_at_U_a = W(1, At(U), a)

  "The window atom wˆ at_U a" should "have one rule with head w_1_at_U_a" in {
    forExactly(1, PlainLarsToAsp.rulesForAt(w_1_at_U_a)) { rule => rule.head.toString should startWith("w_1_at_U_a") }
  }

}
