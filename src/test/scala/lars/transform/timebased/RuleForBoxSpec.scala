package lars.transform.timebased

import core.{PinnedAtom, StringValue, Value}
import core.lars.{Box, TimeWindow, WindowAtom}
import lars.transform.TransformLarsSpec
import org.scalatest.Matchers._
import org.scalatest.Inspectors._


/**
  * Created by FM on 05.05.16.
  */
class RuleForBoxSpec extends TransformLarsSpec {

  def rulesForBox(windowAtom: WindowAtom) = allWindowRules(DefaultLarsToPinnedProgram.slidingTime(windowAtom.windowFunction.asInstanceOf[TimeWindow], windowAtom))

  val w_te_1_b_a = WindowAtom(TimeWindow(1), Box, a)

  "The rule for w^1 b a" should "contain now(T)" in {
    (rulesForBox(w_te_1_b_a) flatMap (_.body)) should contain(now(T))
  }
  it should "have head w_te_1_b_a(T)" in {
    forAtLeast(1, rulesForBox(w_te_1_b_a)) { r => r.head.toString should include("w_te_1_b_a") }
  }
  it should "contain a" in {
    forAtLeast(1, rulesForBox(w_te_1_b_a)) { r => r.body should contain(a) }
  }
  it should "contain a(T - 1)" in {
    forAtLeast(1, rulesForBox(w_te_1_b_a)) { r => r.body should contain(PinnedAtom.asPinnedAtAtom(a, T - 1)) }
  }

  "The rule for w^3 b a" should "contain a(T) a(T -1), a(T -2), a(T -3)" in {
    (rulesForBox(WindowAtom(TimeWindow(3), Box, a)) flatMap (_.body)) should contain allOf(
      a,
      PinnedAtom.asPinnedAtAtom(a, T - 1),
      PinnedAtom.asPinnedAtAtom(a, T - 2),
      PinnedAtom.asPinnedAtAtom(a, T - 3)
    )
  }

  val w_te_1_b_a_1 = WindowAtom(TimeWindow(1), Box, a(StringValue("1")))
  "The rule for w^1 b a(1)" should "have head w_te_1_b_a(1)" in {
    forAtLeast(1, rulesForBox(w_te_1_b_a_1)) { rule =>
      val head = rule.head
      head.toString should include("w_te_1_b_a")

    }
    forAtLeast(1, rulesForBox(w_te_1_b_a_1)) { rule =>
      val head = rule.head
      head.arguments should contain (Value("1"))
    }
  }
}
