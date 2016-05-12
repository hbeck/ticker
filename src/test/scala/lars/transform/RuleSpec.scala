package lars.transform

import core.Atom
import core.asp.AspFact
import core.lars._
import engine.TransformLars
import org.scalatest.Matchers._

/**
  * Created by FM on 07.05.16.
  */
class RuleSpec extends TransformLarsSpec {
  "A fact" should "be transformed into two rules" in {
    val f = Fact(a)

    TransformLars(f, t1) should have size (2)
  }
  it should "contain now(t1) as fact" in {
    val f = Fact(a)

    TransformLars(f, t1) should contain(AspFact(TransformLars.now(t1.toString)))
  }
  it should "contain the fact a(T)." in {
    val f = Fact(a)

    TransformLars(f, t1) should contain(AspFact(a(TransformLars.T)))
  }

  "A rule containing a window atom wˆ1 b a" should "be transformed into three rules" in {
    val r = Rule(b, Set(WindowAtom(SlidingTimeWindow(1), Box, a)))

    TransformLars(r, t1) should have size 3
  }
  it should "contain a rule with head w_1_b_a" in {
    val r = Rule(b, Set(WindowAtom(SlidingTimeWindow(1), Box, a)))

    TransformLars(r, t1).map(_.head) should contain(Atom("w_1_b_a"))
  }
  it should "contain a rule with head w_1_b_a mapped from neg." in {
    val r = Rule(b, Set(), Set(WindowAtom(SlidingTimeWindow(1), Box, a)))

    TransformLars(r, t1).map(_.head) should contain(Atom("w_1_b_a"))
  }

  "A rule containing a window atom aˆ1 d a" should "be transformed into four rules"in{
    val r = Rule(b, Set(WindowAtom(SlidingTimeWindow(1), Diamond, a)))

    TransformLars(r, t1) should have size 4
  }

  "A rule containing a window atom aˆ1 at_1 a" should "be transformed into four rules"in{
    val r = Rule(b, Set(WindowAtom(SlidingTimeWindow(1), At(t1), a)))

    TransformLars(r, t1) should have size 4
  }
}
