package lars.plain

import core.asp.AspProgram
import core.grounding.GrounderInstance
import core.lars._
import engine.asp.PlainLarsToAspMapper
import lars.transform.TransformLarsSpec

/**
  * Created by fm on 21/01/2017.
  */
class ProgramContainingSlidingWindows extends TransformLarsSpec {

  val converter = PlainLarsToAspMapper()

  val program = LarsProgram.from(
    a <= WindowAtom(SlidingTimeWindow(2), Diamond, b)
  )

  "a :- wˆ2 d b" should "be transformed into 7 rules" in {
    val converted = converter.apply(program)

    assert(converted.rules.size == 7)
  }

  it should "be partial grounded into 8 rules" in {
    val converted = converter.apply(program)

    val inspection = ProgramInspection.forAsp(AspProgram(converted.rules.toList))
    val grounder = GrounderInstance.forAsp()
    val groundedRules = converted.rules flatMap grounder.groundWith(inspection)

    assert(groundedRules.size == 8)
  }

}
