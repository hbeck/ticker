package lars.transform

import core.asp.{AspFact, AspRule}
import core.lars._
import core.{Atom, PinnedAtom}
import org.scalatest.Matchers._

/**
  * Created by FM on 07.05.16.
  */
class RuleSpec extends TransformLarsSpec {
  "A fact" should "be transformed into one rule" in {
    val f = LarsFact(a)

    Set(DefaultLarsToPinnedProgram.encodeRule(f).aspRule) should have size (1)
  }

  it should "contain the fact a." in {
    val f = LarsFact(a)

    Set(DefaultLarsToPinnedProgram.encodeRule(f).aspRule) should contain(AspFact(a))
  }

  "A rule containing a window atom wˆ1 b a" should "be transformed into 2 rules" in {
    val r = UserDefinedLarsRule(b, Set(WindowAtom(SlidingTimeWindow(1), Box, a)))

    encodeRule(r) should have size 2
  }

  private def encodeRule(rule: LarsRule) = {
    DefaultLarsToPinnedProgram.encodeRule(rule).windowAtomEncoders.flatMap(allWindowRules)
  }

  it should "contain a rule with head w_1_b_a" in {
    val r = UserDefinedLarsRule(b, Set(WindowAtom(SlidingTimeWindow(1), Box, a)))

    encodeRule(r).map(_.head) should contain(Atom("w_te_1_b_a"))
  }
  it should "contain a rule with head w_1_b_a mapped from neg." in {
    val r = UserDefinedLarsRule(b, Set(), Set(WindowAtom(SlidingTimeWindow(1), Box, a)))

    encodeRule(r).map(_.head) should contain(Atom("w_te_1_b_a"))
  }

  "A rule containing a window atom aˆ1 d a" should "be transformed into 3 rules" in {
    val r = UserDefinedLarsRule(b, Set(WindowAtom(SlidingTimeWindow(1), Diamond, a)))

    encodeRule(r) should have size 2
  }

  "A rule containing a window atom aˆ1 at_1 a" should "be transformed into 3 rules" in {
    val r = UserDefinedLarsRule(b, Set(WindowAtom(SlidingTimeWindow(1), At(t1), a)))

    encodeRule(r) should have size 2
  }

  "A rule containing a window atom aˆ0 d a" should "be transformed into 2 rules" in {
    val r = UserDefinedLarsRule(b, Set(WindowAtom(SlidingTimeWindow(0), Diamond, a)))

    encodeRule(r) should have size 1
  }

}
