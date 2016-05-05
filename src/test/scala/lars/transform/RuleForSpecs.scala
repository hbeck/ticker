package lars.transform

import core.lars.{Diamond, SlidingTimeWindow, WindowAtom, AtAtom}
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

  "A window atom" should "be transformed into some rule" in {
    assert(TransformLars.ruleFor(WindowAtom(SlidingTimeWindow(1), Diamond, a)).isDefined)
  }
}
