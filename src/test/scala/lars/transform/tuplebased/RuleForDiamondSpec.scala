package lars.transform.tuplebased

import core.Atom
import core.lars.{Diamond, SlidingTimeWindow, SlidingTupleWindow, WindowAtom}
import engine.asp.LarsToPinnedProgram
import lars.transform.TransformLarsSpec
import org.scalatest.Inspectors._
import org.scalatest.Matchers._

/**
  * Created by FM on 09.05.16.
  */
class RuleForDiamondSpec extends TransformLarsSpec {
  def a_TUPLE(pos: Int) = Atom("a").asTupleReference(pos)

  val w_tu_2_d_a = WindowAtom(SlidingTupleWindow(2), Diamond, a)

  "The rule for w^2 d a" should "return two rules" in {
    DefaultLarsToPinnedProgram.bodiesForDiamond(w_tu_2_d_a) should have size (2)
  }
  it should "contain now(T) in all rules" in {
    forAll(DefaultLarsToPinnedProgram.bodiesForDiamond(w_tu_2_d_a)) { rule => rule.body should contain(now(T)) }
  }
  it should "have head w_tu_1_b_a" in {
    forAll(DefaultLarsToPinnedProgram.bodiesForDiamond(w_tu_2_d_a)) { rule => rule.head.toString should include("w_tu_2_d_a") }
  }
  it should "contain a_TUPLE(0) for one element" in {
    forExactly(1, DefaultLarsToPinnedProgram.bodiesForDiamond(w_tu_2_d_a)) { rule => rule.body should contain(a_TUPLE(0)) }
  }
  it should "contain a_TUPLE(1)" in {
    forExactly(1, DefaultLarsToPinnedProgram.bodiesForDiamond(w_tu_2_d_a)) { rule => rule.body should contain(a_TUPLE(1)) }
  }

  "The rule for w^3 d a" should "contain a_TUPLE(1), a_TUPLE(2),  a_TUPLE(0)" in {
    DefaultLarsToPinnedProgram.bodiesForDiamond(WindowAtom(SlidingTupleWindow(3), Diamond, a)).
      flatMap(_.body) should contain.
      allOf(a_TUPLE(0), a_TUPLE(1), a_TUPLE(2))
  }
}
