package core.grounding

import core._
import core.asp.{AspProgram, NormalProgram, NormalRule}

/**
  * Created by hb on 08.03.17.
  */
case class AspGrounding(program: NormalProgram) extends OneShotGrounding[NormalProgram,NormalRule,Atom,Atom] {

  val inspect = ProgramInspection.forAsp(program)
  val grounder = GrounderInstance.forAsp()
  val preparedRuleGrounder = grounder.groundWith(inspect) _
  val groundRules = program.rules flatMap preparedRuleGrounder
  val groundProgram = AspProgram(groundRules.toList)

}
