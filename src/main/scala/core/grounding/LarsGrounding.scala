package core.grounding

import core.lars.{ExtendedAtom, HeadAtom, LarsProgram, LarsRule}

/**
  * applicable only to lars programs without "@"
  *
  * Created by hb on 8/21/16.
  */
object LarsGrounding {

  def apply(program: LarsProgram): LarsGrounding = {
    val inspect: StaticProgramInspection[LarsRule, HeadAtom, ExtendedAtom] = StaticProgramInspection.forLars(program)
    val grounder: RuleGrounder[LarsRule, HeadAtom, ExtendedAtom] = GrounderInstance.forLars(inspect)
    val groundRules: Set[LarsRule] = program.rules flatMap (grounder.ground(_)) toSet
    val groundProgram: LarsProgram = LarsProgram.from(groundRules)
    LarsGrounding(inspect,grounder,groundRules,groundProgram)
  }
}

case class LarsGrounding(inspect: StaticProgramInspection[LarsRule, HeadAtom, ExtendedAtom],
                         grounder: RuleGrounder[LarsRule, HeadAtom, ExtendedAtom],
                         groundRules: Set[LarsRule],
                         groundProgram: LarsProgram) {

}

