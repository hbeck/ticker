package lars.transform

import core._
import core.lars._
import engine.asp.LarsToPinnedProgram
import org.scalatest.Matchers._

/**
  * Created by FM on 23.08.16.
  */
class RuleForFluentSpec extends TransformLarsSpec {
  val a_FLUENT = AtomWithArgument(Predicate("a_FLUENT"), Seq())

  val w_fl_d_a = WindowAtom(FluentWindow, Diamond, a)

  "The rule for w^f d a" should "contain now(T)" in {
    (DefaultLarsToPinnedProgram.bodiesForDiamond(w_fl_d_a) flatMap (_.body)) should contain(now(T))
  }
  it should "generate only one rule" in {
    DefaultLarsToPinnedProgram.bodiesForDiamond(w_fl_d_a) should have size (1)
  }

  it should "have head w_fl_d_a(T)" in {
    DefaultLarsToPinnedProgram.bodiesForDiamond(w_fl_d_a).head.head.toString should include("w_fl_d_a")
  }
  it should "contain a_FLUENT" in {
    (DefaultLarsToPinnedProgram.bodiesForDiamond(w_fl_d_a) flatMap (_.body)) should contain(a_FLUENT)
  }

  it should "contain only 2 elements in the body" in {
    DefaultLarsToPinnedProgram.bodiesForDiamond(w_fl_d_a).head.body should have size (2)
  }

  val w_fl_d_a_1 = WindowAtom(FluentWindow, Diamond, a(StringValue("1")))
  "The rule for w^f d a(1)" should "have head w_fl_d_a(1, T)" in {
    val head = DefaultLarsToPinnedProgram.bodiesForDiamond(w_fl_d_a_1).head.head
    head.toString should include("w_fl_d_a")
    head.arguments should contain inOrderOnly(StringValue("1"), T)
  }

  it should "contain in body a_FLUENT(1)" in {
    (DefaultLarsToPinnedProgram.bodiesForDiamond(w_fl_d_a_1) flatMap (_.body)) should contain(a_FLUENT(StringValue("1")))
  }


  val w_fl_d_a_M_on = WindowAtom(FluentWindow, Diamond, a(Variable("M"), StringValue("on")))
  "The rule for w^f d a(M, on)" should "have head w_fl_d_a(M, T)" in {
    val head = DefaultLarsToPinnedProgram.bodiesForDiamond(w_fl_d_a_M_on).head.head
    head.toString should include("w_fl_d_a")
    head.arguments should contain inOrderOnly(Variable("M"), T)
  }

  it should "contain in body a_FLUENT(M, on)" in {
    (DefaultLarsToPinnedProgram.bodiesForDiamond(w_fl_d_a_M_on) flatMap (_.body)) should contain(a_FLUENT(Variable("M"), StringValue("on")))
  }

  val w_fl_d_a_M_on_bar = WindowAtom(FluentWindow, Diamond, a(Variable("M"), StringValue("on"), StringValue("bar")))

  "The rule for w^f d a(M, on, bar)" should "have head w_fl_d_a(M, T)" in {
    val head = DefaultLarsToPinnedProgram.bodiesForDiamond(w_fl_d_a_M_on_bar).head.head
    head.toString should include("w_fl_d_a")
    head.arguments should contain inOrderOnly(Variable("M"), T)
  }

  it should "contain in body a_FLUENT(M, on, bar)" in {
    (DefaultLarsToPinnedProgram.bodiesForDiamond(w_fl_d_a_M_on_bar) flatMap (_.body)) should contain(a_FLUENT(Variable("M"), StringValue("on"), StringValue("bar")))
  }

}
