package core.grounding

import core.lars.{ExtendedAtom, HeadAtom, LarsProgram, LarsRule}

/**
  * applicable only to lars programs without "@"
  *
  * Created by hb on 8/21/16.
  */
object LarsGrounding {

  var useFixedPointCalc = true

  def apply(program: LarsProgram): LarsGrounding = {

    var inspect: StaticProgramInspection[LarsRule, HeadAtom, ExtendedAtom] = null
    var grounder: RuleGrounder[LarsRule, HeadAtom, ExtendedAtom] = null
    var currRules: Set[LarsRule] = program.rules.toSet

    var prevGroundRules: Set[LarsRule] = Set()
    var currGroundRules: Set[LarsRule] = Set()

    var fixedPoint=false
    while (!fixedPoint) {
      inspect = StaticProgramInspection.forLars(currRules.toSeq)
      grounder = GrounderInstance.forLars(inspect)
      currGroundRules = currRules.flatMap(grounder.ground(_))
      if (useFixedPointCalc) {
        if (currGroundRules == prevGroundRules) {
          fixedPoint = true
        } else {
          prevGroundRules = currGroundRules
          currRules = currRules ++ currGroundRules
        }
      } else {
        fixedPoint = true
      }

    }

    val groundProgram: LarsProgram = LarsProgram.from(currGroundRules)

    LarsGrounding(inspect,grounder,currGroundRules,groundProgram)
  }
}

case class LarsGrounding(inspect: StaticProgramInspection[LarsRule, HeadAtom, ExtendedAtom],
                         grounder: RuleGrounder[LarsRule, HeadAtom, ExtendedAtom],
                         groundRules: Set[LarsRule],
                         groundProgram: LarsProgram) {

}

