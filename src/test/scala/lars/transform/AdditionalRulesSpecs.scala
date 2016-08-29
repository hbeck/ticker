package lars.transform

import core.lars._
import engine.asp.LarsToPinnedProgram

/**
  * Created by FM on 05.05.16.
  */
class AdditionalRulesSpecs extends TransformLarsSpec {

  "An atom" should "not be transformed into a rule" in {
    assert(LarsToPinnedProgram.additionalRules(a).isEmpty)
  }

  "An At-atom" should "not be transformed into a rule" in {
    assert(LarsToPinnedProgram.additionalRules(AtAtom(t1, a)).isEmpty)
  }

  "A diamond-window atom" should "be transformed into some rule" in {
    assert(LarsToPinnedProgram.additionalRules(WindowAtom(SlidingTimeWindow(1), Diamond, a)).nonEmpty)
  }
  "A box-window atom" should "be transformed into some rule" in {
    assert(LarsToPinnedProgram.additionalRules(WindowAtom(SlidingTimeWindow(1), Box, a)).nonEmpty)
  }
  "A fluent-window atom" should "be transformed into some rule" in {
    assert(LarsToPinnedProgram.additionalRules(Fluent(a)).nonEmpty)
  }
}
