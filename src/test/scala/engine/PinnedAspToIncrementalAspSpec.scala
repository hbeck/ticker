package engine


import core.asp.AspRule
import core.lars._
import core.{not => _, _}
import engine.asp.{AllRulesAtomEncoder, PlainLarsToAspMapper, now}
import core.asp.{AspFact, AspRule}
import core.lars.{Diamond, LarsProgram, UserDefinedLarsRule, W}
import engine.asp.tms.PinnedAspToIncrementalAsp
import fixtures.TimeTestFixtures
import org.scalatest.FlatSpec
import org.scalatest.Inspectors._
import org.scalatest.Matchers._

import scala.concurrent.duration._

/**
  * Created by FM on 08.06.16.
  */
class PinnedAspToIncrementalAspSpec extends FlatSpec with TimeTestFixtures {
  def LarsToPinnedProgram: PlainLarsToAspMapper = engine.asp.PlainLarsToAspMapper(1 second)

  "A rule containing a normal Atom" should "not be modified" in {
    val rule: AspRule[AtomWithArgument] = AspRule(PinnedAtom(b, t0), PinnedAtom(a, t0))

    PinnedAspToIncrementalAsp(rule, Set()) should be(AspRule(b, PinnedAtom(a, t0)))
  }

  "now(T)" should "be removed from a normal rule" in {
    val r: AspRule[AtomWithArgument] = AspRule[AtomWithArgument, AtomWithArgument](PinnedAtom(a, t0), Set(PinnedAtom(b, t0), now(T)))

    PinnedAspToIncrementalAsp(r, Set()).body should not contain (now(T))
  }

  "The head of a transformed rule" should "not be pinned" in {
    val r: AspRule[AtomWithArgument] = AspRule[AtomWithArgument, AtomWithArgument](PinnedAtom(a, t0), Set(PinnedAtom(b, t0), now(t0)))

    PinnedAspToIncrementalAsp(r, Set()).head shouldBe an[PredicateAtom]
  }

  "Window-Atoms" should "have no pinned head" in {
    val rules = LarsToPinnedProgram.encodeRule(a <= W(1, Diamond, b))
    pending
    //    val converted = rules.map(PinnedAspToIncrementalAsp.apply(_, Set()))
    //    forAll(converted)(r => r.head shouldBe an[PredicateAtom])
  }
  it should "be pinned for an @_U atom" in {
    val rules = LarsToPinnedProgram.encodeRule(a <= W(1, At(U), b))
    pending
    //    val converted = rules.map(PinnedAspToIncrementalAsp.apply(_, Set()))
    //    forExactly(2, converted)(r => r.head.variables should contain(T))
  }
  it should "contain now(t) for an @_t atom" in {
    val rules = LarsToPinnedProgram.encodeRule(a <= W(1, At(t1), b))
    pending
    //    val converted = rules.map(PinnedAspToIncrementalAsp.apply(_, Set()))
    //    forExactly(2, converted)(r => r.body map (_.predicate) should contain(now.predicate))
  }

  "The head of a rule containing @_U" should "still be pinned" in {
    val rules = LarsToPinnedProgram.encodeRule(AtAtom(U, a) <= b)
    pending
    //    val converted = rules.map(PinnedAspToIncrementalAsp.apply(_, Set()))
    //    forAll(converted)(r => r.head.variables should contain(U))
  }

  "The usage of the window-atom body" should "not be pinned" in {
    val windowAtom = W(1, Diamond, b)
    val p = LarsProgram.from(a <= windowAtom)
    val mappedProgram = LarsToPinnedProgram(p)

    val converted = PinnedAspToIncrementalAsp(mappedProgram)

    forAll(converted.rules)(r => r.body should not contain (LarsToPinnedProgram.windowAtomEncoder(windowAtom).asInstanceOf[AllRulesAtomEncoder].allWindowRules))
  }

  "A rule where an atom is part of the head of another rule" should "be unpinned" in {
    val p = LarsProgram.from(
      a <= b,
      c <= a
    )

    val mappedProgram = LarsToPinnedProgram(p)

    val converted = PinnedAspToIncrementalAsp(mappedProgram)

    forAll(converted.rules)(r => r.body should not contain LarsToPinnedProgram.encodingAtom(a))

  }


}
