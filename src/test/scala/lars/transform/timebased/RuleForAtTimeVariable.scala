package lars.transform.timebased

import core.lars._
import core.{Atom, PinnedAtom, Variable}
import lars.transform.TransformLarsSpec
import org.scalatest.Inspectors._
import org.scalatest.Matchers._

/**
  * Created by FM on 16.05.16.
  */
class RuleForAtTimeVariable extends TransformLarsSpec {

  def rulesForAtTimeVariable(windowAtom: WindowAtom) = DefaultLarsToPinnedProgram.slidingTime(windowAtom.windowFunction.asInstanceOf[SlidingTimeWindow], windowAtom).allWindowRules

  val w_te_1_at_U_a = W(1, At(U), a)

  "The rule for w^1 at_1 a" should "return 3 rules" in {
    rulesForAtTimeVariable(w_te_1_at_U_a) should have size (2)
  }
  it should "contain now(T) in all rules" in {
    forAll(rulesForAtTimeVariable(w_te_1_at_U_a)) {
      rule => rule.body should contain(now(T))
    }
  }

  it should "return two rules with head w_1_at_1_a" in {
    forExactly(2, rulesForAtTimeVariable(w_te_1_at_U_a)) {
      rule => rule.head.toString should startWith("w_te_1_at_U_a")
    }
  }
  it should "return  reach_w_te_1_at_U_a(T + 1, T)" in {
    forExactly(1, rulesForAtTimeVariable(w_te_1_at_U_a)) {
      rule => headArguments(rule.head) should contain inOrder(TimeVariableWithOffset("T", -1), TimeVariableWithOffset("T"))
    }
  }
  it should "return  reach_w_te_1_at_U_a(T, T)" in {
    forExactly(1, rulesForAtTimeVariable(w_te_1_at_U_a)) {
      rule => headArguments(rule.head) should contain theSameElementsInOrderAs Seq(TimeVariableWithOffset("T"), TimeVariableWithOffset("T"))
    }
  }

  it should "have tow heads with Time-Variables T,T" in {
    forExactly(1, rulesForAtTimeVariable(w_te_1_at_U_a)) {
      rule => headArguments(rule.head) should contain inOrder(T - 1, T)
    }
  }

  it should "contain a(T) for one element" in {
    forExactly(1, rulesForAtTimeVariable(w_te_1_at_U_a)) { rule => rule.body should contain(a(T)) }
  }

  "The rule for w^2 at_U a" should "have rules with heads w_te_2_at_U_a(T-2,T), w_te_2_at_U_a(T-1, T), w_te_2_at_U_a(T,T), w_te_2_at_U_a(U,T)" in {
    val windowAtom = Atom("w_te_2_at_U_a")
    val body = rulesForAtTimeVariable(WindowAtom(SlidingTimeWindow(2), At(U), a)) map (_.head)
    body should contain allOf(
      windowAtom(T - 2)(T),
      windowAtom(T - 1)(T),
      windowAtom(T)(T)
    )
  }


  def headArguments(atom: Atom) = atom match {
    case p: PinnedAtom => p.arguments
    case _ => {
      val Atom(arguments) = atom

      arguments
    }
  }

}
