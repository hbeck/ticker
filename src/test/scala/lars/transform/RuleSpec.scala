package lars.transform

import core.Atom
import core.asp.{AspRule, AspFact}
import core.lars._
import engine.PlainLarsToAsp
import org.scalatest.Matchers._

/**
  * Created by FM on 07.05.16.
  */
class RuleSpec extends TransformLarsSpec {
  "A fact" should "be transformed into two rules" in {
    val f = Fact(a)

    PlainLarsToAsp(f) should have size (1)
  }

  it should "contain the fact a(T)." in {
    val f = Fact(a)

    PlainLarsToAsp(f) should contain(AspRule(a(T), Set(now(T))))
  }

  "A rule containing a window atom wˆ1 b a" should "be transformed into 2 rules" in {
    val r = Rule(b, Set(WindowAtom(SlidingTimeWindow(1), Box, a)))

    PlainLarsToAsp(r) should have size 2
  }
  it should "contain a rule with head w_1_b_a" in {
    val r = Rule(b, Set(WindowAtom(SlidingTimeWindow(1), Box, a)))

    PlainLarsToAsp(r).map(_.head) should contain(Atom("w_1_b_a"))
  }
  it should "contain a rule with head w_1_b_a mapped from neg." in {
    val r = Rule(b, Set(), Set(WindowAtom(SlidingTimeWindow(1), Box, a)))

    PlainLarsToAsp(r).map(_.head) should contain(Atom("w_1_b_a"))
  }

  "A rule containing a window atom aˆ1 d a" should "be transformed into 3 rules" in {
    val r = Rule(b, Set(WindowAtom(SlidingTimeWindow(1), Diamond, a)))

    PlainLarsToAsp(r) should have size 3
  }

  "A rule containing a window atom aˆ1 at_1 a" should "be transformed into 3 rules" in {
    val r = Rule(b, Set(WindowAtom(SlidingTimeWindow(1), At(t1), a)))

    PlainLarsToAsp(r) should have size 3
  }
}