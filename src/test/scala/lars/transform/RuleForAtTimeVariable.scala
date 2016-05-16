package lars.transform

import core.lars._
import core.{Atom, AtomWithArguments}
import engine.asp.PlainLarsToAsp
import org.scalatest.Inspectors._
import org.scalatest.Matchers._

/**
  * Created by FM on 16.05.16.
  */
class RuleForAtTimeVariable extends TransformLarsSpec {

  val w_1_at_U_a = W(1, At(U), a)

  "The rule for w^1 at_1 a" should "return 3 rules" in {
    PlainLarsToAsp.rulesForAtTimeVariable(w_1_at_U_a, U) should have size (3)
  }
  it should "contain now(T) in all rules" in {
    forAll(PlainLarsToAsp.rulesForAtTimeVariable(w_1_at_U_a, U)) {
      rule => rule.body should contain(now(T))
    }
  }

  it should "return two rules with head reach_w_1_at_U_a" in {
    forExactly(2, PlainLarsToAsp.rulesForAtTimeVariable(w_1_at_U_a, U)) { rule => rule.head.toString should startWith("reach_w_1_at_U_a") }
  }
  it should "return  reach_w_1_at_U_a(T -1, T)" in {
    forExactly(1, PlainLarsToAsp.rulesForAtTimeVariable(w_1_at_U_a, U)) {
      rule => headArguments(rule.head) should contain inOrder("T - 1", "T")
    }
  }
  it should "return  reach_w_1_at_U_a(T, T)" in {
    forExactly(1, PlainLarsToAsp.rulesForAtTimeVariable(w_1_at_U_a, U)) {
      rule => headArguments(rule.head) should contain theSameElementsInOrderAs Seq("T", "T")
    }
  }

  it should "have head w_1_at_1_a for one rule" in {
    forExactly(1, PlainLarsToAsp.rulesForAtTimeVariable(w_1_at_U_a, U)) { rule => rule.head.toString should startWith("w_1_at_U_a") }
  }
  it should "have one head with Time-Variables U,T" in {
    forExactly(1, PlainLarsToAsp.rulesForAtTimeVariable(w_1_at_U_a, U)) {
      rule => headArguments(rule.head) should contain inOrder(U.toString, T.toString)
    }
  }

  it should "contain a(U) for one element" in {
    forExactly(1, PlainLarsToAsp.rulesForAtTimeVariable(w_1_at_U_a, U)) { rule => rule.body should contain(a(U)) }
  }
  it should "contain reach_w_1_at_U_a(U,T) for one element" in {
    val reach = Atom("reach_w_1_at_U_a")(U)
    forExactly(1, PlainLarsToAsp.rulesForAtTimeVariable(w_1_at_U_a, U)) { rule => rule.body should contain(reach(T)) }
  }

  "The rule for w^2 at_U a" should "have rules with heads reach_w_2_at_U_a(T-2,T), reach_w_2_at_U_a(T-1, T), reach_w_2_at_U_a(T,T), reach_w_2_at_U_a(U,T)" in {
    val reach = Atom("reach_w_2_at_U_a")
    val body = PlainLarsToAsp.rulesForAtTimeVariable(WindowAtom(SlidingTimeWindow(2), At(U), a), U) map (_.head)
    body should contain allOf(
      reach(T - 2)(T),
      reach(T - 1)(T),
      reach(T)(T),
      Atom("w_2_at_U_a")(U)(T)
      )
  }


  def headArguments(atom: Atom) = {
    val Atom(arguments) = atom

    arguments
  }

}
