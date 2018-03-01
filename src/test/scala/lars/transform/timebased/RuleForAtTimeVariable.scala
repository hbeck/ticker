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

  def rulesForAtTimeVariable(windowAtom: WindowAtom) = allWindowRules(DefaultLarsToPinnedProgram.slidingTime(windowAtom.windowFunction.asInstanceOf[TimeWindow], windowAtom))

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
  it should "return  reach_w_te_1_at_U_a(T + 1)" in {
    forExactly(1, rulesForAtTimeVariable(w_te_1_at_U_a)) {
      rule => headArguments(rule.head) should contain(TimeVariableWithOffset(T.variable, -1))
    }
  }
  it should "return  reach_w_te_1_at_U_a(T)" in {
    forExactly(1, rulesForAtTimeVariable(w_te_1_at_U_a)) {
      rule => headArguments(rule.head) should contain theSameElementsInOrderAs Seq(TimeVariableWithOffset(T.variable))
    }
  }

  it should "have two heads with Time-Variables T" in {
    forExactly(1, rulesForAtTimeVariable(w_te_1_at_U_a)) {
      rule => headArguments(rule.head) should contain(T - 1)
    }
  }

  it should "contain a(T) for one element" in {
    forExactly(1, rulesForAtTimeVariable(w_te_1_at_U_a)) { rule => rule.body should contain(PinnedAtom.asPinnedAtAtom(a, T)) }
  }

  "The rule for w^2 at_U a" should "have rules with heads w_te_2_at_U_a(T-2), w_te_2_at_U_a(T-1), w_te_2_at_U_a(T), w_te_2_at_U_a(U)" in {
    def windowAtom(t: Time) = PinnedAtom.asPinnedAtAtom(Atom("w_te_2_at_U_a"), t)

    val body = rulesForAtTimeVariable(WindowAtom(TimeWindow(2), At(U), a)) map (_.head)
    body should contain allOf(
      windowAtom(T - 2),
      windowAtom(T - 1),
      windowAtom(T)
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
