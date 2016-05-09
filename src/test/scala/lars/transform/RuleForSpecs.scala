package lars.transform

import core.lars._
import engine.TransformLars

/**
  * Created by FM on 05.05.16.
  */
class RuleForSpecs extends TransformLarsSpec {

  "An atom" should "not be transformed into a rule" in {
    assert(TransformLars.ruleFor(a).isEmpty)
  }

  "An At-atom" should "not be transformed into a rule" in {
    assert(TransformLars.ruleFor(AtAtom(t1, a)).isEmpty)
  }

  "A diamond-window atom" should "be transformed into some rule" in {
    assert(TransformLars.ruleFor(WindowAtom(SlidingTimeWindow(1), Diamond, a)).nonEmpty)
  }
  "A box-window atom" should "be transformed into some rule" in {
    assert(TransformLars.ruleFor(WindowAtom(SlidingTimeWindow(1), Box, a)).nonEmpty)
  }
}
