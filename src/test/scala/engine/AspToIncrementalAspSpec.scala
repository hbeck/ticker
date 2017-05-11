package engine


import core.asp.AspRule
import core.lars.{Diamond, LarsProgram, W, _}
import core.{not => _, _}
import engine.asp.PlainLarsToAspMapper
import engine.asp.tms.IncrementalAspPreparation
import fixtures.TimeTestFixtures
import org.scalatest.FlatSpec
import org.scalatest.Inspectors._
import org.scalatest.Matchers._

import scala.concurrent.duration._

/**
  * Created by FM on 08.06.16.
  */
class AspToIncrementalAspSpec extends FlatSpec with TimeTestFixtures {
  def LarsToAspProgram: PlainLarsToAspMapper = engine.asp.PlainLarsToAspMapper(1 second)

  "A rule containing a normal Atom" should "not be modified" in {
    val rule: AspRule[AtomWithArguments] = AspRule(PinnedAtom(b, t0), PinnedAtom(a, t0))

    IncrementalAspPreparation(rule, Set()) should be(AspRule(b, PinnedAtom(a, t0)))
  }

  "Window-Atoms" should "have no pinned head" in {
    val rules = LarsToAspProgram.encodeRule(a <= W(1, Diamond, b))
    pending
    //    val converted = rules.map(AspToIncrementalAsp.apply(_, Set()))
    //    forAll(converted)(r => r.head shouldBe an[PredicateAtom])
  }
  it should "be pinned for an @_U atom" in {
    val rules = LarsToAspProgram.encodeRule(a <= W(1, At(U), b))
    pending
    //    val converted = rules.map(AspToIncrementalAsp.apply(_, Set()))
    //    forExactly(2, converted)(r => r.head.variables should contain(T))
  }
  it should "contain now(t) for an @_t atom" in {
    val rules = LarsToAspProgram.encodeRule(a <= W(1, At(t1), b))
    pending
    //    val converted = rules.map(AspToIncrementalAsp.apply(_, Set()))
    //    forExactly(2, converted)(r => r.body map (_.predicate) should contain(now.predicate))
  }

  "The head of a rule containing @_U" should "still be pinned" in {
    val rules = LarsToAspProgram.encodeRule(AtAtom(U, a) <= b)
    pending
    //    val converted = rules.map(AspToIncrementalAsp.apply(_, Set()))
    //    forAll(converted)(r => r.head.variables should contain(U))
  }

  "The usage of the window-atom body" should "not be pinned" in {
    val windowAtom = W(1, Diamond, b)
    val p = LarsProgram.from(a <= windowAtom)
    val mappedProgram = LarsToAspProgram(p)

    val converted = IncrementalAspPreparation(mappedProgram)

    forAll(converted.rules)(r => r.body should not contain (LarsToAspProgram.windowAtomEncoder(windowAtom).allWindowRules))
  }

  "A rule where an atom is part of the head of another rule" should "be unpinned" in {
    val p = LarsProgram.from(
      a <= b,
      c <= a
    )

    val mappedProgram = LarsToAspProgram(p)

    val converted = IncrementalAspPreparation(mappedProgram)

    forAll(converted.rules)(r => r.body should not contain LarsToAspProgram.encodingAtom(a))

  }


}
