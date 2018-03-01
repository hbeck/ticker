package lars.transform.tuplebased

import core.{Atom, IntValue, PinnedAtom, StringVariable}
import core.lars._
import lars.transform.TransformLarsSpec
import org.scalatest.Inspectors._
import org.scalatest.Matchers._

/**
  * Created by FM on 09.05.16.
  */
class RuleForDiamondSpec extends TransformLarsSpec {
  override val T = TimeVariableWithOffset(StringVariable("TT"))

  def a_TUPLE(pos: Int) = PinnedAtom.asPinnedAtCntAtom(Atom("a"), T, C - pos)

  val w_tu_2_d_a = WindowAtom(TupleWindow(2), Diamond, a)

  "The rule for w^2 d a" should "return two rules" in {
    allWindowRules(DefaultLarsToPinnedProgram.slidingTuple(TupleWindow(2), w_tu_2_d_a)) should have size (3)
  }
  it should "contain now(T) in all rules" in {
    forAll(allWindowRules(DefaultLarsToPinnedProgram.slidingTuple(TupleWindow(2), w_tu_2_d_a))) { rule => rule.body should contain(cnt(C)) }
  }
  it should "have head w_tu_1_b_a" in {
    forAll(allWindowRules(DefaultLarsToPinnedProgram.slidingTuple(TupleWindow(2), w_tu_2_d_a))) { rule => rule.head.toString should include("w_tu_2_d_a") }
  }
  it should "contain a_TUPLE(0) for one element" in {
    forExactly(1, allWindowRules(DefaultLarsToPinnedProgram.slidingTuple(TupleWindow(2), w_tu_2_d_a))) { rule => rule.body should contain(a_TUPLE(0)) }
  }
  it should "contain a_TUPLE(1)" in {
    forExactly(1, allWindowRules(DefaultLarsToPinnedProgram.slidingTuple(TupleWindow(2), w_tu_2_d_a))) { rule => rule.body should contain(a_TUPLE(1)) }
  }

  "The rule for w^3 d a" should "contain a_TUPLE(1), a_TUPLE(2),  a_TUPLE(0)" in {
    allWindowRules(DefaultLarsToPinnedProgram.slidingTuple(TupleWindow(3), WindowAtom(TupleWindow(3), Diamond, a))).
      flatMap(_.body) should contain.
      allOf(a_TUPLE(0), a_TUPLE(1), a_TUPLE(2))
  }
}
