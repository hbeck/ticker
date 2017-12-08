package lars.transform

import core.PinnedAtom
import core.asp.{AspFact, AspRule}
import core.lars.{AtAtom, LarsFact, UserDefinedLarsRule}
import org.scalatest.Matchers._

/**
  * Created by FM on 07.05.16.
  */
class RuleOnlySpec extends TransformLarsSpec {

  "A fact" should "be translated in a fact with now(T) in the body" in {
    val f = LarsFact(a)

    DefaultLarsToPinnedProgram.encode(f).body should not contain (now(T))
  }

  it should "transform an at-atom in the head" in {
    val f = LarsFact(AtAtom(t1, a))
    assert(DefaultLarsToPinnedProgram.encode(f) == AspFact(PinnedAtom.asPinnedAtAtom(a, t1)))
  }

  "A rule containing only positive elements" should "have the same amount of items in its transformed pos. body and now(T)" in {
    val r = UserDefinedLarsRule(a, Set(b, c))
    DefaultLarsToPinnedProgram.encode(r).pos should have size 2
  }
  "A rule containing only negative elements" should "have the same amount of items in its transformed neg. body" in {
    val r = UserDefinedLarsRule(a, Set(), Set(b, c))
    DefaultLarsToPinnedProgram.encode(r).neg should have size 2
  }
}

