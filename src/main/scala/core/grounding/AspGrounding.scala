package core.grounding

import core.asp.{AspProgram, NormalProgram}

/**
  * Created by hb on 08.03.17.
  */
case class AspGrounding(program: NormalProgram) {

  val grounder = GrounderInstance.oneShotAsp(StaticProgramInspection.forAsp(program))
  val groundRules = program.rules flatMap grounder.ground
  val groundProgram = AspProgram(groundRules.toList)

}
