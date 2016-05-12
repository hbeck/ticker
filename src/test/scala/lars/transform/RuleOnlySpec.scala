package lars.transform

import core.asp.AspFact
import core.lars.{Rule, AtAtom, Fact}
import engine.PlainLarsToAsp
import org.scalatest.Matchers._

/**
  * Created by FM on 07.05.16.
  */
class RuleOnlySpec extends TransformLarsSpec {
  "A fact" should "be translated in an fact" in {
    val f = Fact(a)

    assert(PlainLarsToAsp.rule(f).body.isEmpty)
  }
  it should "transform an at-atom in the head" in {
    val f = Fact(AtAtom(t1, a))
    assert(PlainLarsToAsp.rule(f) == AspFact(a(t1.toString)))
  }

  "A rule containing only positive elements" should "have the same amount of items in its transformed pos. body" in {
    val r = Rule(a, Set(b, c))
    PlainLarsToAsp.rule(r).pos should have size 2
  }
  "A rule containing only negative elements" should "have the same amount of items in its transformed neg. body" in {
    val r = Rule(a, Set(), Set(b, c))
    PlainLarsToAsp.rule(r).neg should have size 2
  }
}

