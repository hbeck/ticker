package lars.transform

import core.lars._

/**
  * Created by FM on 05.05.16.
  */
class AdditionalRulesSpecs extends TransformLarsSpec {

  "A diamond-window atom" should "be transformed into some rule" in {
    assert(DefaultLarsToPinnedProgram.windowAtomEncoder(WindowAtom(SlidingTimeWindow(1), Diamond, a)).allWindowRules.nonEmpty)
  }
  "A box-window atom" should "be transformed into some rule" in {
    assert(DefaultLarsToPinnedProgram.windowAtomEncoder(WindowAtom(SlidingTimeWindow(1), Box, a)).allWindowRules.nonEmpty)
  }
  "A fluent-window atom" should "be transformed into some rule" in {
    assert(DefaultLarsToPinnedProgram.windowAtomEncoder(Fluent(a)).allWindowRules.nonEmpty)
  }
}
