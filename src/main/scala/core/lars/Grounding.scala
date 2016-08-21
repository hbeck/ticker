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
    if (inspect isGround rule) return Set(rule)
    val possibleValuesPerVariable: Map[Variable, Set[Value]] = inspect possibleValuePerVariable rule
    val assignments: Set[Assignment] = flatten(possibleValuesPerVariable)
    var groundRules = Set[LarsRule]()
    for (assignment <- assignments) {
      groundRules = groundRules + ground(rule,assignment)
    }
    groundRules
  }

  def ground(rule: LarsRule, assignment: Assignment): LarsRule = {
    val groundHead: HeadAtom = groundAtom(rule.head, assignment)
    val groundPosBody = rule.pos map (x => groundAtom(x, assignment))
    val groundNegBody = rule.neg map (x => groundAtom(x, assignment))
    LarsRule(groundHead,groundPosBody,groundNegBody)
    LarsRule(Atom("a"),Set(),Set())
  }

  def groundAtom[T <: ExtendedAtom](extAtom: T, assignment: Assignment): T = {
    if (extAtom.isGround) return
    extAtom match {
      case a:HeadAtom =>
      case => Atom("a")
    }
  }

*/

}


case class LarsProgramInspection(p: LarsProgram) {

}