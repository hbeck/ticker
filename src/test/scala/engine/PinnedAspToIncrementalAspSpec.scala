package engine

import core.asp.AspRule
import core.lars._
import core.{not => _, _}
import engine.asp.{PlainLarsToAsp, now}
import core.asp.{AspFact, AspRule}
import core.lars.{Diamond, LarsProgram, UserDefinedLarsRule, W}
import engine.asp.now
import engine.asp.PlainLarsToAsp
import engine.asp.tms.PinnedAspToIncrementalAsp
import fixtures.TimeTestFixtures
import org.scalatest.FlatSpec
import org.scalatest.Inspectors._
import org.scalatest.Matchers._

/**
  * Created by FM on 08.06.16.
  */
class PinnedAspToIncrementalAspSpec extends FlatSpec with TimeTestFixtures {

  "A rule containing a normal Atom" should "not be modified" in {
    val rule: AspRule[AtomWithArgument] = AspRule(PinnedAtom(b, t0), PinnedAtom(a, t0))

    PinnedAspToIncrementalAsp(rule, Set()) should be(AspRule(b, PinnedAtom(a, t0)))
  }

  "now(T)" should "be removed from a rule" in {
    val r: AspRule[AtomWithArgument] = AspRule(PinnedAtom(a, t0), Set(PinnedAtom(b, t0), now(t0)))

    PinnedAspToIncrementalAsp(r, Set()).body should not contain (now(t0))
  }

  "The head of a transformed rule" should "not be pinned" in {
    val r: AspRule[AtomWithArgument] = AspRule(PinnedAtom(a, t0), Set(PinnedAtom(b, t0), now(t0)))

    PinnedAspToIncrementalAsp(r, Set()).head shouldBe an[PredicateAtom]
  }

  "Window-Atoms" should "have no pinned head" in {
    val rules = PlainLarsToAsp(a <= W(1, Diamond, b))

    val converted = rules.map(PinnedAspToIncrementalAsp.apply(_, Set()))
    forAll(converted)(r => r.head shouldBe an[PredicateAtom])
  }

  "The usage of the window-atom body" should "not be pinned" in {
    val windowAtom = W(1, Diamond, b)
    val p = LarsProgram.from(a <= windowAtom)
    val mappedProgram = PlainLarsToAsp(p)

    val converted = PinnedAspToIncrementalAsp(mappedProgram)

    forAll(converted.rules)(r => r.body should not contain (PlainLarsToAsp.apply(windowAtom)))
  }

  "A rule where an atom is part of the head of another rule" should "be unpinned" in {
    val p = LarsProgram.from(
      a <= b,
      c <= a
    )

    val mappedProgram = PlainLarsToAsp(p)

    val converted = PinnedAspToIncrementalAsp(mappedProgram)

    forAll(converted.rules)(r => r.body should not contain PlainLarsToAsp.apply(a))

  }


}
