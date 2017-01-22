package lars.plain

import core.Atom
import core.asp.NormalRule
import core.lars._
import engine.asp.PlainLarsToAsp
import lars.transform.TransformLarsSpec

/**
  * Created by fm on 21/01/2017.
  */
class ProgramContainingSlidingWindows extends TransformLarsSpec {

  val converter = PlainLarsToAsp()

  val program = LarsProgram.from(
    a <= WindowAtom(SlidingTimeWindow(2), Diamond, b)
  )

  "a :- wˆ2 d b" should "be transformed into 7 rules" in {
    val converted = converter.apply(program)

    assert(converted.rules.size == 7)
  }

  it should "be partial grounded into 8 rules" in {
    val converted = converter.apply(program)

    val inspection = LarsProgramInspection.from(converted.rules)
    val grounder = new GroundRule[NormalRule, Atom, Atom]
    val groundedRules = converted.rules flatMap grounder.ground(inspection)

    assert(groundedRules.size == 8)
  }

}
