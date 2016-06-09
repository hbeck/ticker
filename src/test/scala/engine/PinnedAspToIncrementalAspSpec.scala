package engine

import core.{Atom, PinnedAtom, Predicate}
import core.asp.{AspFact, AspRule}
import core.lars.{Diamond, LarsProgram$, LarsRule, W}
import engine.asp.now
import engine.asp.{PinnedAspToIncrementalAsp, PlainLarsToAsp}
import fixtures.TimeTestFixtures
import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.Inspectors._

/**
  * Created by FM on 08.06.16.
  */
class PinnedAspToIncrementalAspSpec extends FlatSpec with TimeTestFixtures {

  "A rule containing a normal Atom" should "not be modified" in {
    val rule = AspRule(PinnedAtom(b, t0), PinnedAtom(a, t0))

    PinnedAspToIncrementalAsp(rule, Set()) should be(AspRule(b, PinnedAtom(a, t0)))
  }

  "now(T)" should "be removed from a rule" in {
    val r = AspRule(PinnedAtom(a, t0), Set(PinnedAtom(b, t0), now(t0)))

    PinnedAspToIncrementalAsp(r, Set()).body should not contain (now(t0))
  }

  "The head of a transformed rule" should "not be pinned" in {
    val r = AspRule(PinnedAtom(a, t0), Set(PinnedAtom(b, t0), now(t0)))

    PinnedAspToIncrementalAsp(r, Set()).head shouldBe an[Predicate]
  }

  "Window-Atoms" should "have no pinned head" in {
    val rules = PlainLarsToAsp(a <= W(1, Diamond, b))

    val converted = rules.map(PinnedAspToIncrementalAsp.apply(_, Set()))
    forAll(converted)(r => r.head shouldBe an[Predicate])
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
