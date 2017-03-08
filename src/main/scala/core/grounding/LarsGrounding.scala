package core.grounding

import core.lars.{ExtendedAtom, HeadAtom, LarsProgram, LarsRule}

/**
  * applicable only to lars programs without "@"
  *
  * Created by hb on 8/21/16.
  */
case class LarsGrounding(program: LarsProgram) extends OneShotGrounding[LarsProgram, LarsRule, HeadAtom, ExtendedAtom] {

  val inspect = ProgramInspection.forLars(program)
  val grounder = GrounderInstance.forLars()
  val preparedRuleGrounder = grounder.groundWith(inspect) _
  val groundRules = program.rules flatMap preparedRuleGrounder
  val groundProgram = LarsProgram(groundRules)

}
