package lars.transform.timebased

import core.lars._
import core.{Atom, Variable}
import engine.asp.LarsToPinnedProgram
import lars.transform.TransformLarsSpec
import org.scalatest.Inspectors._
import org.scalatest.Matchers._

/**
  * Created by FM on 16.05.16.
  */
class RuleForAtTimeVariable extends TransformLarsSpec {

  val w_te_1_at_U_a = W(1, At(U), a)

  "The rule for w^1 at_1 a" should "return 3 rules" in {
    DefaultLarsToPinnedProgram.rulesForAtTimeVariable(w_te_1_at_U_a, U) should have size (3)
  }
  it should "contain now(T) in all rules" in {
    forAll(DefaultLarsToPinnedProgram.rulesForAtTimeVariable(w_te_1_at_U_a, U)) {
      rule => rule.body should contain(now(T))
    }
  }

  it should "return two rules with head reach_w_te_1_at_U_a" in {
    forExactly(2, DefaultLarsToPinnedProgram.rulesForAtTimeVariable(w_te_1_at_U_a, U)) { rule => rule.head.toString should startWith("reach_w_te_1_at_U_a") }
  }
  it should "return  reach_w_te_1_at_U_a(T -1, T)" in {
    forExactly(1, DefaultLarsToPinnedProgram.rulesForAtTimeVariable(w_te_1_at_U_a, U)) {
      rule => headArguments(rule.head) should contain inOrder(Variable("T - 1"), Variable("T"))
    }
  }
  it should "return  reach_w_te_1_at_U_a(T, T)" in {
    forExactly(1, DefaultLarsToPinnedProgram.rulesForAtTimeVariable(w_te_1_at_U_a, U)) {
      rule => headArguments(rule.head) should contain theSameElementsInOrderAs Seq(Variable("T"), Variable("T"))
    }
  }

  it should "have head w_1_at_1_a for one rule" in {
    forExactly(1, DefaultLarsToPinnedProgram.rulesForAtTimeVariable(w_te_1_at_U_a, U)) { rule => rule.head.toString should startWith("w_te_1_at_U_a") }
  }
  it should "have one head with Time-Variables U,T" in {
    forExactly(1, DefaultLarsToPinnedProgram.rulesForAtTimeVariable(w_te_1_at_U_a, U)) {
      rule => headArguments(rule.head) should contain inOrder(U.variable, T.variable)
    }
  }

  it should "contain a(U) for one element" in {
    forExactly(1, DefaultLarsToPinnedProgram.rulesForAtTimeVariable(w_te_1_at_U_a, U)) { rule => rule.body should contain(a(U)) }
  }
  it should "contain reach_w_te_1_at_U_a(U,T) for one element" in {
    val reach = Atom("reach_w_te_1_at_U_a")(U)
    forExactly(1, DefaultLarsToPinnedProgram.rulesForAtTimeVariable(w_te_1_at_U_a, U)) { rule => rule.body should contain(reach(T)) }
  }

  "The rule for w^2 at_U a" should "have rules with heads reach_w_te_2_at_U_a(T-2,T), reach_w_te_2_at_U_a(T-1, T), reach_w_te_2_at_U_a(T,T), reach_w_te_2_at_U_a(U,T)" in {
    val reach = Atom("reach_w_te_2_at_U_a")
    val body = DefaultLarsToPinnedProgram.rulesForAtTimeVariable(WindowAtom(SlidingTimeWindow(2), At(U), a), U) map (_.head)
    body should contain allOf(
      reach(T - 2)(T),
      reach(T - 1)(T),
      reach(T)(T),
      Atom("w_te_2_at_U_a")(U)(T)
      )
  }


  def headArguments(atom: Atom) = {
    val Atom(arguments) = atom

    arguments
  }

}
