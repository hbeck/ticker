package engine

import core.{Atom, PinnedAtom}
import fixtures.AtomTestFixture
import org.scalatest.FlatSpec

/**
  * Created by fm on 05/06/2017.
  */
class AtomResultFilterSpec extends FlatSpec with AtomTestFixture {
  "An empty Result" should "be filtered to an empty model" in {
    val filter = AtomResultFilter(Set())
    assert(filter.filter(0, EmptyResult).model.isEmpty)
  }

  "A non empty Result with no allowed Atoms" should "be filtered to an empty model" in {
    val filter = AtomResultFilter(Set())
    assert(filter.filter(0, Result(Set(a))).model.isEmpty)
  }

  "A non empty Result with the same restricted Atoms" should "be filtered to the same model" in {
    val filter = AtomResultFilter(Set(a))
    val result = Result(Set(a))
    assert(filter.filter(0, result).model == result.model)
  }

  "Pinned Atoms at the same timepoint" should "be used with only the atom" in {
    val filter = AtomResultFilter(Set(a))
    val result = Result(Set[Atom](PinnedAtom.asPinnedAtAtom(a, 0)))

    assert(filter.filter(0, result).model == Set(a))
  }

  "Pinned Atoms at a different timepoint" should "be filtered to an empty model" in {
    val filter = AtomResultFilter(Set(a))
    val result = Result(Set[Atom](PinnedAtom.asPinnedAtAtom(a, 0)))

    assert(filter.filter(1, result).model == Set())
  }
}
