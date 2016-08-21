package core.lars

/**
  * Created by hb on 8/21/16.
  */
object Grounding {

  /*
  def apply(program: LarsProgram): LarsProgram = {
    val inspect = LarsProgramInspection(program)
    val groundRules = program.rules flatMap ground(inspect)
    LarsProgram(groundRules)
  }

  def ground(inspect: LarsProgramInspection)(rule: LarsRule): Set[LarsRule] = {
    if (rule isGround) return Set(rule)
    val possibleValuesPerVariable: Map[Variable, Set[Value]] = inspect possibleValuePerVariable rule
    val assignments: Set[Assignment] = createAssignments(possibleValuesPerVariable)
    var groundRules = Set[LarsRule]()
    for (assignment <- assignments) {
      groundRules = groundRules + rule.assign(assignment)
    }
    groundRules
  }
  */

//  def ground(rule: LarsRule, assignment: Assignment): LarsRule = {
//    val groundHead: HeadAtom = ground(rule.head, assignment)
//    val groundPosBody = rule.pos map (x => ground(x, assignment))
//    val groundNegBody = rule.neg map (x => ground(x, assignment))
//    LarsRule(groundHead,groundPosBody,groundNegBody)
//  }

  def ground[T <: ExtendedAtom](x: T, assignment: Assignment): T = {
    if (x.isGround) return x
    x.assign(assignment).asInstanceOf[T]
  }

}


case class LarsProgramInspection(p: LarsProgram) {

}